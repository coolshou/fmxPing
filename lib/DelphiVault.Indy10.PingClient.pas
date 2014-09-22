// Written with Delphi XE3 Pro
// Created Oct 12, 2012 by Darian Miller
// for http://stackoverflow.com/questions/12858551/delphi-xe2-indy-10-mutlithread-ping
unit DelphiVault.Indy10.PingClient;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Generics.Collections,
  IdIcmpClient, IdGlobal;

type
  TThreadedPing = class(TThread)
  private
    // Thread reuse
    //FTickEvent: TEvent;
    // fIndex:integer; //Thread index?
    fPingNumber: integer;
    fDelay: integer; // ms
    fHost: String;
    fTimeout: integer; // ms
    fPacketSize: integer;
    fProtocol: integer;
    fIPVersion: TIdIPVersion; // Id_IPv4, Id_IPv6
    // Thread reuse
    // FWakeEvent: TEvent;
    // FDoneEvent: TEvent;
    //TODO: Ping statics
    fTotalPing:integer;
    fTotalPingFail:integer;
    // FTerminateEvent: TEvent;
  protected
    procedure Execute; override;
    procedure HandlePingResponse(const ReplyStatus: TReplyStatus);
    procedure SynchronizedResponse(const ReplyStatus: TReplyStatus);
      virtual; abstract;
  public
    constructor Create(const HostToPing: String); overload;
    constructor Create(const HostToPing: String; Timeout: integer); overload;
    constructor Create(const HostToPing: String; Timeout: integer;
      PacketSize: integer); overload;
    constructor Create(const HostToPing: String; Timeout: integer;
      PacketSize: integer; IPVersion: TIdIPVersion); overload;
    destructor Destroy; override;
    procedure setTimeout(Timeout: integer);
    procedure setPacketSize(PacketSize: integer);
    procedure setPingNumber(PingNumber: integer);
    procedure setDelay(Delay: integer);
    procedure FinishThreadExecution;
    // published
    // property Timeout: integer read fTimeout write setTimeout;
    // property PacketSize: integer read fPacketSize write setPacketSize;
  end;

  TPingClient = class
  private const
    defReceiveTimeout = 200;
    defPacketSize = 24;
    // Resolve Error 10040: See http://stackoverflow.com/questions/12723081/delphi-indy-ping-error-10040
    defProtocol = 1;
    defIPVersion = Id_IPv4;
  private type
    TSystemPingResponse = procedure(const ReplyStatus: TReplyStatus) of object;

    TCustomPingClient = class
    private
      fClientId: Word;
      fIndyClient: TIdIcmpClient;
      fCallBack: TSystemPingResponse;
    public
      constructor Create();
      destructor Destroy(); override;

      property ClientId: Word read fClientId write fClientId;
      property IndyClient: TIdIcmpClient read fIndyClient write fIndyClient;
      property CallBack: TSystemPingResponse read fCallBack write fCallBack;

      procedure DoPing();
    end;
  private
  class var
    _PingClientList: TDictionary<integer, TCustomPingClient>;
    _LastClientId: Word;
  private
    class procedure InternalHandleReply(Sender: TComponent;
      const ReplyStatus: TReplyStatus);
    class function GetAvailableClientId(): Word;
  public
    class constructor ClassCreate();
    class destructor ClassDestroy();

    class procedure Ping(const HostToPing: String;
      const CallBack: TSystemPingResponse;
      const ReceiveTimeout: integer = defReceiveTimeout;
      const PacketSize: integer = defPacketSize;
      const Protocol: integer = defProtocol;
      const IPVersion: TIdIPVersion = defIPVersion);
    class function FormatStandardResponse(const ReplyStatus
      : TReplyStatus): String;
  end;

implementation

class constructor TPingClient.ClassCreate();
begin
  _PingClientList := TDictionary<integer, TCustomPingClient>.Create();
end;

class destructor TPingClient.ClassDestroy();
begin
  _PingClientList.Free();
end;

constructor TPingClient.TCustomPingClient.Create();
begin
  inherited;
  fIndyClient := TIdIcmpClient.Create(nil);
end;

destructor TPingClient.TCustomPingClient.Destroy();
begin
  fIndyClient.Free();
  inherited;
end;

procedure TPingClient.TCustomPingClient.DoPing();
begin
  // Indy's core problem...

  // We will send here, but if we have recently sent some Pings, we may receive
  // a response to a previous Ping and not get a response to our particular Ping.
  IndyClient.Ping('', ClientId);
end;

class function TPingClient.GetAvailableClientId(): Word;
begin
  //High(Word) = 65535
  while _LastClientId < High(Word) do
  begin
    Inc(_LastClientId);
    if not _PingClientList.ContainsKey(_LastClientId) then
    begin
      Exit(_LastClientId);
    end;
  end;

  _LastClientId := Low(Word);

  while _LastClientId < High(Word) do
  begin
    Inc(_LastClientId);
    if not _PingClientList.ContainsKey(_LastClientId) then
    begin
      Exit(_LastClientId);
    end;
  end;

  raise Exception.Create('Too many concurrent PING operations');
end;

// serialized ping calls
class procedure TPingClient.Ping(const HostToPing: String;
  const CallBack: TSystemPingResponse;
  const ReceiveTimeout: integer = defReceiveTimeout;
  const PacketSize: integer = defPacketSize;
  const Protocol: integer = defProtocol;
  const IPVersion: TIdIPVersion = defIPVersion);
var
  Transport: TCustomPingClient;
begin
  Transport := TCustomPingClient.Create();
  Transport.IndyClient.Host := HostToPing;
  Transport.IndyClient.ReceiveTimeout := ReceiveTimeout;
  Transport.IndyClient.PacketSize := PacketSize;
  Transport.IndyClient.Protocol := Protocol;
  Transport.IndyClient.IPVersion := IPVersion;
  Transport.IndyClient.OnReply := InternalHandleReply;
  Transport.CallBack := CallBack;

  System.TMonitor.Enter(_PingClientList);
  try
    Transport.ClientId := GetAvailableClientId();
    _PingClientList.Add(Transport.ClientId, Transport);
    Transport.DoPing();
  finally
    System.TMonitor.Exit(_PingClientList);
  end;
end;

// serialized response handling
class procedure TPingClient.InternalHandleReply(Sender: TComponent;
  const ReplyStatus: TReplyStatus);
var
  Transport: TCustomPingClient;
  PingID: Word;
begin
  PingID := ReplyStatus.SequenceId;
  System.TMonitor.Enter(_PingClientList);
  try
    if _PingClientList.TryGetValue(PingID, Transport) then
    begin
      try
        Transport.CallBack(ReplyStatus);
        _PingClientList.Remove(PingID);
      finally
        Transport.Free();
      end;
    end;
  finally
    System.TMonitor.Exit(_PingClientList);
  end;
end;

class function TPingClient.FormatStandardResponse(const ReplyStatus
  : TReplyStatus): String;
begin
  if ReplyStatus.ReplyStatusType = rsEcho then
  begin
    Result := Format('%d bytes receive from %s: icmp_seq=%d ttl=%d time %d ms',
      [ReplyStatus.BytesReceived, ReplyStatus.FromIpAddress,
      ReplyStatus.SequenceId, ReplyStatus.TimeToLive,
      ReplyStatus.MsRoundTripTime]);
  end
  else if Length(ReplyStatus.Msg) > 0 then
  begin
    Result := ReplyStatus.Msg;
  end
  else
  begin
    Result := 'Ping failed, error code ' +
      IntToStr(Ord(ReplyStatus.ReplyStatusType));
  end;
end;

constructor TThreadedPing.Create(const HostToPing: String);
begin
//  inherited Create(False);
  //CreateSuspended
  inherited Create(true);
  // FreeOnTerminate := True;
  FreeOnTerminate := False;
  // FTickEvent := TEvent.Create(nil, True, False, intTostr(self.ThreadID), True);
  //FTickEvent := TEvent.Create(nil, True, False, IntToStr(self.ThreadID), False);
  // FTickEvent := CreateEvent(nil, True, False, nil);
  // FDoneEvent := TEvent.Create(NIL, False, False, '');
  // FWakeEvent := TEvent.Create(NIL, False, False, '');

  fPingNumber := 1;
  fDelay := 1000;
  fHost := HostToPing;
  fTimeout := 1000;
  // Win7 default 32
  fPacketSize := 32;
  fProtocol := 1;
  fIPVersion := Id_IPv4; // Id_IPv4, Id_IPv6
  // Resume;
end;

constructor TThreadedPing.Create(const HostToPing: String; Timeout: integer);
begin
//  inherited Create(False);
  Create(HostToPing);
  fTimeout := Timeout;
end;

constructor TThreadedPing.Create(const HostToPing: String; Timeout: integer;
  PacketSize: integer);
begin
//  inherited Create(False);
  Create(HostToPing);
  fTimeout := Timeout;
  fPacketSize := PacketSize;
end;

constructor TThreadedPing.Create(const HostToPing: String; Timeout: integer;
  PacketSize: integer; IPVersion: TIdIPVersion);
begin
//  inherited Create(False);
  Create(HostToPing);
  fTimeout := Timeout;
  fPacketSize := PacketSize;
  fIPVersion := IPVersion;
end;

destructor TThreadedPing.Destroy;
begin
  // FDoneEvent.Free;
  // FWakeEvent.Free;
  // CloseHandle(FTickEvent);
//  FTickEvent.Free;
  inherited;
end;

procedure TThreadedPing.Execute;
begin
  inherited;
  {
    while not Terminated do
    begin
    // wait for work unit
    if FWakeEvent.WaitFor(INFINITE) <> wrSignaled then
    Exit;
    if Terminated then
    Exit;
    try
    // FMethod(FData);
    // do work
    TPingClient.Ping(fHost, HandlePingResponse, fTimeout, fPacketSize,
    fProtocol, fIPVersion);
    except
    on E: EAbort do
    begin
    end; // ignore
    // on E: TObject do FException := EMyException.CreateFromException(E);
    end;
    // signal that work is done
    FDoneEvent.SetEvent;
    end;
  }

  // TPingClient.Ping(fHost, HandlePingResponse, fTimeout, fPacketSize);
  TPingClient.Ping(fHost, HandlePingResponse, fTimeout, fPacketSize, fProtocol,
    fIPVersion);
  while not Terminated do
  begin
    // let main thread do something
    // if (FTickEvent.WaitFor(2000)= wrTimeout) then
    // begin
    // fmPingMain.Caption := IntToStr(fmPingMain.Tag);
    Yield();
    // end;
  end;
  {
    while not Terminated do
    begin
    if WaitForSingleObject(FTickEvent, 2000) = WAIT_TIMEOUT then
    begin
    Synchronize(
    procedure
    begin
    //Form1.Tag := Form1.Tag + 1;
    //Form1.Caption := IntToStr(Form1.Tag);
    end);
    end;
    end;
  }
end;

// Callback function of TPingClient.Ping
procedure TThreadedPing.HandlePingResponse(const ReplyStatus: TReplyStatus);
begin
  fPingNumber:= fPingNumber-1;
  //TODO: ping count?
  Synchronize(
    procedure
    begin
      SynchronizedResponse(ReplyStatus);
    end
  );
  // delay time
  sleep(fDelay);
  // after handle ping response,Terminate TThreadedPing?re-usable
  if (fPingNumber <> 0) then
  begin
    //continious ping or still left ping number
    Execute;
  end else
  begin
    Suspended:=true;
//    self
    //TODO: user should manual stop thread
//
//    self.WaitFor(1000);
//    self.
   // SetSuspended();
//    Terminate; //this cause thread Terminate
  end;

end;

procedure TThreadedPing.setTimeout(Timeout: integer);
begin
  fTimeout := Timeout;
end;

procedure TThreadedPing.setPacketSize(PacketSize: integer);
begin
  fPacketSize := PacketSize;
end;

//
procedure TThreadedPing.setPingNumber(PingNumber: integer);
var
  numPingNumber: integer;
begin
  if PingNumber <= 0 then
    numPingNumber := 1
  else
    numPingNumber := PingNumber;
  fPingNumber := numPingNumber;
end;

procedure TThreadedPing.setDelay(Delay: integer);
var
  numDelay: integer;
begin
  if (Delay < 1000) then
    numDelay := 1000
  else
    numDelay := Delay;
  fDelay := numDelay;
end;

procedure TThreadedPing.FinishThreadExecution;
begin
  Terminate;
  // FWakeEvent.SetEvent; // necessary to exit thread loop
  // SetEvent(FTickEvent);
//  FTickEvent.SetEvent;
  WaitFor;
end;

end.
