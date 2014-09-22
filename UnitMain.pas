unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.SyncObjs, System.Rtti, System.Actions,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ActnList, FMX.StdActns, FMX.Menus, FMX.TabControl,
  FMX.Layouts, FMX.Memo, FMX.TreeView, FMX.Edit, FMX.ListBox, FMX.Grid,
  FMX.Objects,
  Xml.xmldom, Xml.XMLIntf, Xml.adomxmldom, Xml.XMLDoc,
  // Xml.Win.msxmldom,
  IdBaseComponent,
  IdComponent, IdRawBase, IdRawClient, IdIcmpClient, IdIPAddress, IdGlobal,
  DelphiVault.Indy10.PingClient,
  uCiaXml, uDialogs;

type
  {
    //  TPingReplyData = class(TObject)
    TPingReplyData = class(TObject)
    FPingResult: boolean;
    //TDateTime;
    FDateTime: string;
    FHostName: string;
    FIPAddress: string;
    FStatus: string;
    end;
  }
  // TODO: let mypingthread keep ping at specify interval
  TMyPingThread = class(TThreadedPing)
  protected
    // thread id?
    procedure SynchronizedResponse(const ReplyStatus: TReplyStatus); override;
  end;

  TfmPingMain = class(TForm)
    MainMenu1: TMainMenu;
    mitFile: TMenuItem;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    TabControl1: TTabControl;
    ActConfig: TAction;
    mitOption: TMenuItem;
    MItemFileExit: TMenuItem;
    TabItPing: TTabItem;
    TabItReport: TTabItem;
    TabItStatic: TTabItem;
    Panel1: TPanel;
    PingList: TMemo;
    ActStart: TAction;
    ActStop: TAction;
    ActClear: TAction;
    mitCommand: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
    tvStatic: TTreeView;
    ActFileOpen: TAction;
    MItemFileOpen: TMenuItem;
    ConfigLoad: TAction;
    ConfigSave: TAction;
    configFile: TXMLConfig;
    spInterval: TSpinBox;
    ToolBar1: TToolBar;
    StatusBar1: TStatusBar;
    spTimeoutTime: TSpinBox;
    spPacketSize: TSpinBox;
    spCriteria: TSpinBox;
    spPingLostSerialNum: TSpinBox;
    lbInterval: TLabel;
    lbTimeoutTime: TLabel;
    lbPacketSize: TLabel;
    PingTimer: TTimer;
    MemoReport: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    tvReport: TTreeView;
    StyleBook1: TStyleBook;
    MItemConfigSave: TMenuItem;
    spPingNumber: TSpinBox;
    lbPingNumber: TLabel;
    spDelay: TSpinBox;
    lbDelay: TLabel;
    lbTTL: TLabel;
    spTTL: TSpinBox;
    cbPeriodically: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ConfigLoadExecute(Sender: TObject);
    procedure ConfigSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ActStartExecute(Sender: TObject);
    procedure PingListChange(Sender: TObject);
    procedure PingTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActStopExecute(Sender: TObject);
    procedure ActClearExecute(Sender: TObject);
    procedure spIntervalChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbPeriodicallyChange(Sender: TObject);
    procedure spPingNumberChange(Sender: TObject);
    procedure spDelayChange(Sender: TObject);
    procedure spPacketSizeChange(Sender: TObject);
    procedure spTimeoutTimeChange(Sender: TObject);
    procedure spTTLChange(Sender: TObject);
  private
    { Private declarations }
    FCanClose: boolean;
    procedure setStartStopTag(bStart: boolean);
    procedure setPingNumberTag(bPingNumber: boolean);
    procedure setIntervalTag(bInterval: boolean);
    procedure DoApplyPingResponseItemStyleLookup(Sender: TObject);
  public
    { Public declarations }
    function getPingList(): string;
    procedure setPingList(s: string);
    procedure AddPingResponse(ReplyStatus: TReplyStatus);
    procedure setModify(bModify: boolean);
  end;

var
  fmPingMain: TfmPingMain;
  isModify: boolean;
  pingListObj: TStringList;

const
  myDateTimeString = 'YYYYMMDD_hh:mm:ss.zzz';
  // pingResponseNG = 'seq %d: %s: %s [%d]';
  // pingResponseOK = 'seq %d: Reply from %s: bytes=%d TTL=%d [%d]';
  pingResponseNG = ' ERROR: %s [%d]';
  pingResponseOK = ' bytes=%d TTL=%d Time=%d [%d]';

implementation

{$R *.fmx}

uses UnitFuncs, TreeViewPingItem, UntMain;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.DelimitedText := Str;
end;

// this procedure only work on  fmPingMain
// TODO: add parent Tform for messageDlg
procedure DoAfterCloseQuery(const MR: TModalResult; dlg: TObject);
begin
  case MR of
    mrYes:
      begin
        fmPingMain.ConfigSaveExecute(fmPingMain);
      end;
    // mrNo : Form1.Memo1.Lines.Add('¿ï¾Ü:§_');
  end;
  fmPingMain.FCanClose := true;
  TMyCustomDialog(dlg).Free;
  fmPingMain.Close;
end;

// ==============================================================================
procedure TMyPingThread.SynchronizedResponse(const ReplyStatus: TReplyStatus);
begin
  // Example2: Do something special with ReplyStatus. Here, we're just displaying
  fmPingMain.AddPingResponse(ReplyStatus);
end;

// ==============================================================================

procedure TfmPingMain.ConfigLoadExecute(Sender: TObject);
begin
  // XMLDocConfig.
  // ping
  // edtHost.Text := configFile.ReadString('PING', 'host', '192.168.0.1');
  setPingList(configFile.ReadString('PING', 'hosts', '192.168.0.1'));
  cbPeriodically.IsChecked := configFile.ReadBoolean('PING', 'Periodically', true);
  spInterval.Text := configFile.ReadString('PING', 'Interval', '1');
  spTimeoutTime.Text := configFile.ReadString('PING', 'TimeoutTime', '2000');
  spPacketSize.Text := configFile.ReadString('PING', 'PacketSize', '1024');
  spPingNumber.Text := configFile.ReadString('PING', 'PingNumber', '5');
  spDelay.Text := configFile.ReadString('PING', 'Delay', '2000');
  spTTL.Text := configFile.ReadString('PING', 'TTL', '255');
  spCriteria.Text := configFile.ReadString('PING', 'PingCriteria', '1');
  spPingLostSerialNum.Text := configFile.ReadString('PING',
    'PingLostSerialNum', '10');
  {
    // log
    cbEnableLogFile.Checked := configFile.ReadBoolean('LOG', 'enableLog', true);
    edLogfileName.Text := configFile.ReadString('LOG', 'LogfileName', 'test.log');
    cbEnableSubffix.Checked := configFile.ReadBoolean('LOG',
    'enableSubffix', false);
    edSubffixString.Text := configFile.ReadString('LOG', 'SubffixString',
    'YYYY-MM-DD_hhmmss');
    // chart
    cbEnableChart.Checked := configFile.ReadBoolean('Chart',
    'EnableChart', false);
    // report
    myServer.cbRunAsServer.Checked := configFile.ReadBoolean('REPORT',
    'RunAsServer', false);
    myServer.cbReportToServer.Checked := configFile.ReadBoolean('REPORT',
    'ReportToServer', false);
    myServer.edServerIP.Text := configFile.ReadString('REPORT', 'ServerIP',
    '192.168.0.1');
    myServer.spReportInterval.Value := configFile.ReadInteger('REPORT',
    'ReportInterval', 300);
  }
  setModify(false);
end;

procedure TfmPingMain.ConfigSaveExecute(Sender: TObject);
begin

  // ping
  // configFile.WriteString('PING', 'host', edtHost.Text);
  // save PingList's all text (include #)
  configFile.WriteString('PING', 'hosts', PingList.Lines.Text);
  // configFile.WriteString('PING', 'hosts', PingList.Lines.Text);
  configFile.WriteBoolean('PING', 'Periodically', cbPeriodically.IsChecked);
  configFile.WriteString('PING', 'Interval', spInterval.Text);
  configFile.WriteString('PING', 'TimeoutTime', spTimeoutTime.Text);
  configFile.WriteString('PING', 'PacketSize', spPacketSize.Text);
  configFile.WriteString('PING', 'PingNumber', spPingNumber.Text);
  configFile.WriteString('PING', 'Delay', spDelay.Text);
  configFile.WriteString('PING', 'TTL', spTTL.Text);
  configFile.WriteString('PING', 'PingCriteria', spCriteria.Text);
  configFile.WriteString('PING', 'PingLostSerialNum', spPingLostSerialNum.Text);
  // report
  {
    configFile.WriteBoolean('LOG', 'enableLog', cbEnableLogFile.Checked);
    configFile.WriteString('LOG', 'LogfileName', edLogfileName.Text);
    configFile.WriteBoolean('LOG', 'enableSubffix', cbEnableSubffix.Checked);
    configFile.WriteString('LOG', 'SubffixString', edSubffixString.Text);
    // chart
    configFile.WriteBoolean('Chart', 'EnableChart', cbEnableChart.Checked);
    // reportServer
    configFile.WriteBoolean('REPORT', 'RunAsServer',
    myServer.cbRunAsServer.Checked);
    configFile.WriteBoolean('REPORT', 'ReportToServer',
    myServer.cbReportToServer.Checked);
    configFile.WriteString('REPORT', 'ServerIP', myServer.edServerIP.Text);
    configFile.WriteInteger('REPORT', 'ReportInterval',
    myServer.spReportInterval.Value);
  }
  configFile.Save;

end;

procedure TfmPingMain.setPingList(s: string);
var
  OutPutList: TStringList;
  I: integer;
  t: string;
  AStream: TStringStream;
begin
  AStream := TStringStream.Create(s);
  OutPutList := TStringList.Create;
  try
    PingList.Lines.Clear;
    // load all text in xml string to stream
    OutPutList.LoadFromStream(AStream);
    for I := 0 to OutPutList.Count - 1 do
    begin
      t := trim(OutPutList.Strings[I]);
      if t <> '' then
      begin
        PingList.Lines.Add(t);
      end;
    end;
  finally
    OutPutList.Free;
    OutPutList:=nil;
    AStream.Free;
    AStream:=nil;
  end;

end;

// return IP address string in PingList
function TfmPingMain.getPingList(): string;
var
  t, rs: string;
  I: integer;
begin
  rs := '';
  for I := 0 to (PingList.Lines.Count - 1) do
  begin
    t := PingList.Lines[I];
    // remove empty line
    t := trim(t);
    if (t <> '') then
    begin
      if not CharInSet(t[1], ['#']) then
      begin
        // remove duplicate IP address
        if (Pos(t, rs) <= 0) then
        begin
          // TODO: check result of getPingList is correct IPv4 or IPv6 address
          //
          rs := rs + ' ' + t;
        end;
      end;
    end;
  end;
  result := rs;
end;

procedure TfmPingMain.PingListChange(Sender: TObject);
begin
//  setModify(true);
  // TODO: check input is valid IPv4 or IPv6 address
  // Ignore #
  // locate current line
  // LineNumber := Coordinate.Y + 1;
  // PingList.PosToTextPos();
  // PingList.TextPosToPos();
  { VCL
    With Memo1 do begin
    Line := Perform(EM_LINEFROMCHAR,SelStart, 0) ;
    Column := SelStart - Perform(EM_LINEINDEX, Line, 0) ;
    end;
  }
end;

procedure TfmPingMain.PingTimerTimer(Sender: TObject);
var
  I: integer;
  mpt: TMyPingThread;
begin
  // Start to ping all hosts
  for I := 0 to pingListObj.Count - 1 do
  begin
    // pingListObj.Strings[I];
    mpt := TMyPingThread(pingListObj.Objects[I]);
    // if mpt.CheckTerminated then
    if not mpt.Started then
    begin
      if mpt.Terminated then
      begin
        // running or suspending thread??
        mpt.Start;
      end;
    end;
    // TODO: when ping timeout > interval will cause ping not execute at exact time
    {
      if not mpt.Started then
      begin
      MemoReport.Lines.Add('Ping ' + pingListObj.Strings[I]);
      if not mpt.Suspended then
      begin
      mpt.Start;
      end;
      end;
    }
  end;

end;

procedure TfmPingMain.setModify(bModify: boolean);
begin
  // ignore when already modify
  if (CharInSet(fmPingMain.Caption[1], ['*']) and bModify) then
    exit;

  isModify := bModify;
  if isModify then
  begin
    fmPingMain.Caption := 'fmPing ' + '*';
  end
  else
  begin
    fmPingMain.Caption := 'fmPing';
  end;
  FCanClose := not isModify;
end;

procedure TfmPingMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  pingListObj.Free;
end;

procedure TfmPingMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FCanClose;
  if isModify then
  begin
    // custom info message to save setting
    uDialogs.msg_YesNo(self, 'Save setting before exit?', DoAfterCloseQuery);
  end;

end;

procedure TfmPingMain.FormCreate(Sender: TObject);
begin
{$IFDEF DEBUG}
  // for DEBUG MemoryLeaks
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  configFile.FileName := ChangeFileExt(ParamStr(0), '_cfg.xml');
  configFile.Active := true;
  PingList.OnChangeTracking := nil;
  // load config
  ConfigLoadExecute(self);
  PingList.OnChangeTracking := PingListChange;

  pingListObj := TStringList.Create;
  // tvReport.ShowScrollBars := true;
  PingTimer.Enabled := false;
end;

procedure TfmPingMain.FormDestroy(Sender: TObject);
var
  mpt: TMyPingThread;
  I: integer;
begin
  for I := (pingListObj.Count - 1) downto 0 do
  begin
    mpt := TMyPingThread(pingListObj.Objects[I]);
    mpt.FinishThreadExecution;
  end;
end;

procedure TfmPingMain.setStartStopTag(bStart: boolean);
begin
  ActStart.Enabled := not bStart;
  ActStop.Enabled := bStart;
  ActClear.Enabled := not bStart;

end;

procedure TfmPingMain.setPingNumberTag(bPingNumber: boolean);
begin
  lbPingNumber.Enabled := bPingNumber;
  spPingNumber.Enabled := bPingNumber;
end;

procedure TfmPingMain.setIntervalTag(bInterval: boolean);
begin
  lbInterval.Enabled := bInterval;
  spInterval.Enabled := bInterval;
end;

procedure TfmPingMain.spDelayChange(Sender: TObject);
begin
  //TODO: compare previous state in xml
  setModify(true);

end;

procedure TfmPingMain.spIntervalChange(Sender: TObject);
begin
  //TODO: compare previous state in xml
  setModify(true);
end;

procedure TfmPingMain.spPacketSizeChange(Sender: TObject);
begin
  //TODO: compare previous state in xml
  setModify(true);

end;

procedure TfmPingMain.spPingNumberChange(Sender: TObject);
begin
  //TODO: compare previous state in xml
  setModify(true);
end;

procedure TfmPingMain.spTimeoutTimeChange(Sender: TObject);
begin
  //TODO: compare previous state in xml
  setModify(true);

end;

procedure TfmPingMain.spTTLChange(Sender: TObject);
begin
  //TODO: compare previous state in xml
  setModify(true);

end;

procedure TfmPingMain.ActClearExecute(Sender: TObject);
begin
  MemoReport.Lines.Clear;
  tvReport.Clear;
end;

procedure TfmPingMain.ActStartExecute(Sender: TObject);
var
  I: integer;
  s: string;
  idx: integer;
  ls: TStringList;
  mpt: TMyPingThread;
  bIsIPv4: boolean;
  bIsIPv6: boolean;
  IP: string;
  vIP: string;
  IPToCheck: TIdIPAddress;
begin
  setStartStopTag(true);
  // create ping host list
  s := getPingList();
  ls := TStringList.Create;
  try
    Split(' ', s, ls);
    // generate ping list
    for I := (pingListObj.Count - 1) downto 0 do
    begin
      idx := ls.IndexOf(pingListObj.Strings[I]);
      if idx < 0 then
      begin
        // remove not exist hosts
        pingListObj.Delete(I);
      end;
    end;
    // add new hosts
    for I := 0 to (ls.Count - 1) do
    begin
      bIsIPv4 := false;
      bIsIPv6 := false;
      idx := pingListObj.IndexOf(ls.Strings[I]);
      if idx < 0 then
      begin
        IP := ls.Strings[I];
        mpt := nil;
        IPToCheck := nil;
        // Check Host IP address is IPv4 or IPv6
        // TODO: this function to check vail IPv4 & IPv6 is not confidence
        IPToCheck := TIdIPAddress.MakeAddressObject(IP);
        if IPToCheck <> nil then
          try
            bIsIPv4 := true;
            mpt := TMyPingThread.Create(ls.Strings[I],
              strToInt(spTimeoutTime.Text), strToInt(spPacketSize.Text),
              IPToCheck.AddrType);
          finally
            IPToCheck.Free;
          end;
        // ignore Illegal ip address
        if (bIsIPv4 or bIsIPv6) then
        begin
          pingListObj.AddObject(ls.Strings[I], mpt);
        end;
      end
      else
      begin
        mpt := TMyPingThread(pingListObj.Objects[idx]);
      end;
      // update following value in mpt setting
      mpt.setPingNumber(strToInt(spPingNumber.Text));
      mpt.setDelay(strToInt(spDelay.Text));
      mpt.setTimeout(strToInt(spTimeoutTime.Text));
      mpt.setPacketSize(strToInt(spPacketSize.Text));
      if mpt.Suspended then
      begin
        mpt.Start;
      end;
      // mpt.Start;
      // mpt.SetFreeOnTerminate(false);
    end;
    MemoReport.Lines.Add('### Start Ping');
    // setup timer for ping
    // TODO: let thread keeps running by it self, only detect signal to stop running
    // not using Timer to do so
    // TODO following
    // PingTimer.Interval := (strToInt(spInterval.Text) * 1000);
    // PingTimer.Enabled := true;
  finally
    ls.Free;
  end;
  // show report tab
  TabControl1.TabIndex := 1;
  // TabItReport.SetFocus;
end;

procedure TfmPingMain.ActStopExecute(Sender: TObject);
var
  I: integer;
  mpt: TMyPingThread;
begin
  PingTimer.Enabled := false;
  for I := 0 to pingListObj.Count - 1 do
  begin
    mpt := TMyPingThread(pingListObj.Objects[I]);
    if mpt.Started then
      mpt.Terminate;
  end;
  setStartStopTag(false);
  MemoReport.Lines.Add('### Stop Ping');
end;

procedure TfmPingMain.DoApplyPingResponseItemStyleLookup(Sender: TObject);
var
  itm: TTreeViewPingReportItem;
  img: Timage;
  Obj: TFMXObject;
  // imgStr: string;
begin
  itm := TTreeViewPingReportItem(Sender);
  begin
    Obj := itm.FindStyleResource('image');
    if Obj is Timage then
    begin
      img := Timage(Obj);
      if itm.PingResult then
        img.Bitmap.StyleLookup := 'ImgOK'
      else
        img.Bitmap.StyleLookup := 'ImgFail'
    end;
    itm.StylesData['textDateTime'] := itm.DateTime;
    itm.StylesData['textHost'] := itm.HostName;
    itm.StylesData['textIPaddress'] := itm.IPAddress;
    itm.StylesData['textStatus'] := itm.Status;
  end;
end;

//
procedure TfmPingMain.AddPingResponse(ReplyStatus: TReplyStatus);
var
  itm: TTreeViewPingReportItem;
begin
  MemoReport.Lines.Add(TPingClient.FormatStandardResponse(ReplyStatus));
  // improve report list
  tvReport.BeginUpdate;
  begin
    itm := TTreeViewPingReportItem.Create(self);
    itm.Parent := tvReport;
    itm.OnApplyStyleLookup := DoApplyPingResponseItemStyleLookup;
    itm.StyleLookup := 'tvReportItemStyle';
    if (ReplyStatus.ReplyStatusType = rsEcho) then
    begin
      itm.PingResult := true;
      itm.Status := format(pingResponseOK, [ReplyStatus.BytesReceived,
        ReplyStatus.TimeToLive, ReplyStatus.MsRoundTripTime,
        Ord(ReplyStatus.ReplyStatusType)]);
    end
    else
    begin
      itm.PingResult := false;
      itm.Status := format(pingResponseNG,
        [getIndyReplyMsg(ReplyStatus.ReplyStatusType),
        Ord(ReplyStatus.ReplyStatusType)]);
    end;
    itm.DateTime := formatdatetime(myDateTimeString, now);
    // who receive address
    itm.HostName := ReplyStatus.ToIpAddress;
    // who reply
    itm.IPAddress := ReplyStatus.FromIpAddress;
  end;
  tvReport.EndUpdate;
  // TODO: ping count: Global/each thread count
  // TODO: what to do when all thread is Terminated;

end;

procedure TfmPingMain.cbPeriodicallyChange(Sender: TObject);
begin
  setIntervalTag(cbPeriodically.IsChecked);
  //TODO: compare previous state in xml
  setModify(true);

end;

end.
