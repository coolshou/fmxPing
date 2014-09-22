unit UnitFuncs;

interface

uses
{$IFDEF MACOS}
  MacApi.CoreFoundation, MacApi.Foundation,
{$ENDIF}
{$IFDEF IOS}
  iOSapi.UIKit,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.Os,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, PJSysInfo,
  IpHlpApi, IpTypes,
{$ENDIF}
  System.SysUtils,
  System.Classes,
  IdIcmpClient;

type
  TDeviceInfoRec = record
    OSNameStr: String;
    OSVersionStr: String;
    DeviceTypeStr: String;
    ComputerNameStr:String;
    BoardNameStr:String; //Motherboard name (TODO)
  end;
  //single NIC info(TODO)
  TNICInfoRec = record
    MacAddr: array[0..5] of Byte;
    IPAddress:string;
  end;
  //List of NIC(TODO)
  TNICList = record
    NICList: array of TNICInfoRec;
  end;

{$IFDEF MACOS}
function NSUserName: Pointer; cdecl;
  external '/System/Library/Frameworks/Foundation.framework/Foundation' name
  '_NSUserName';
{$ENDIF}
function getIndyReplyMsg(reply: TReplyStatusTypes): string;
function GetComputerName: string;
function GetUserName: String;
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
{$IFDEF ANDROID}
function GetCodename(VerString: string): string;
{$ENDIF}
function GetDeviceInfo(): TDeviceInfoRec;

implementation

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.DelimitedText := Str;
end;

function getIndyReplyMsg(reply: TReplyStatusTypes): string;
var
  ErrorText: string;
begin
  case reply of
    rsEcho:
      ErrorText := 'An Echo was received.';
    rsError:
      ErrorText := 'An error has occurred.';
    rsTimeOut:
      ErrorText := 'Timeout occurred before a response was received.';
    rsErrorUnreachable:
      ErrorText := 'The address for the ICMP message is not available.';
    rsErrorTTLExceeded:
      ErrorText := 'Time-To-Live exceeded for an ICMP response.';
    rsErrorPacketTooBig:
      ErrorText := 'Packet Too Big';
    rsErrorParameter:
      ErrorText := 'Error Parameter';
    rsErrorDatagramConversion:
      ErrorText := 'Error Datagram Conversion';
    rsErrorSecurityFailure:
      ErrorText := 'Error Security Failure';
    rsSourceQuench:
      ErrorText := 'Source Quench';
    rsRedirect:
      ErrorText := 'Redirect';
    rsTimeStamp:
      ErrorText := 'TimeStamp';
    rsInfoRequest:
      ErrorText := 'InfoRequest';
    rsAddressMaskRequest:
      ErrorText := 'AddressMaskRequest';
    rsTraceRoute:
      ErrorText := 'TraceRoute';
    rsMobileHostReg:
      ErrorText := 'MobileHostReg';
    rsMobileHostRedir:
      ErrorText := 'MobileHostRedir';
    rsIPv6WhereAreYou:
      ErrorText := 'IPv6 Where Are You';
    rsIPv6IAmHere:
      ErrorText := 'IPv6 I Am Here';
    rsSKIP:
      ErrorText := 'Skip';
  else
    ErrorText := 'Unknown Error';
  end;
  result := ErrorText;
end;

function GetComputerName: string;
{$IFDEF MSWINDOWS}
var
  buffer: array [0 .. MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Size: Cardinal;
{$ENDIF}
begin
{$IFDEF MACOS}
  // TODO: MAC OS/Android
  result:='TODO: MAC OS/Android';
{$ENDIF}
{$IFDEF MSWINDOWS}
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  Windows.GetComputerName(@buffer, Size);
  result := StrPas(buffer);
{$ENDIF}
end;

function GetUserName: String;
{$IFDEF MSWINDOWS}
var
  nSize: DWord;
{$ENDIF}
begin
{$IFDEF MACOS}
  result := TNSString.Wrap(NSUserName).UTF8String;
{$ENDIF}
{$IFDEF MSWINDOWS}
  nSize := 1024;
  SetLength(result, nSize);
  if Windows.GetUserName(PChar(result), nSize) then
  begin
    SetLength(result, nSize - 1)
  end
  else
  begin
    RaiseLastOSError;
  end
{$ENDIF}
end;


function GetDeviceInfo(): TDeviceInfoRec;
var
{$IFDEF IOS}
  Device: UIDevice;
  OSNameStr, OSVersionStr, DeviceTypeStr: string;
{$ENDIF}
  DeviceInfoRec: TDeviceInfoRec;
begin
{$IFDEF IOS}
  Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
  DeviceInfoRec.OSNameStr := Device.systemName.UTF8String;
  DeviceInfoRec.OSVersionStr := Device.systemVersion.UTF8String;
  DeviceInfoRec.DeviceTypeStr := Device.model.UTF8String;

  DeviceInfoRec.ComputerNameStr := GetComputerName;
  DeviceInfoRec.BoardNameStr := 'TODO(IOS)';
{$ENDIF}
{$IFDEF ANDROID}
  DeviceInfoRec.OSNameStr:= GetCodename(JStringToString(TJBuild_VERSION.JavaClass.RELEASE));
  DeviceInfoRec.OSVersionStr:= JStringToString(TJBuild_VERSION.JavaClass.RELEASE);
  DeviceInfoRec.DeviceTypeStr:= JStringToString(TJBuild.JavaClass.model);

  DeviceInfoRec.ComputerNameStr := GetComputerName;
  DeviceInfoRec.BoardNameStr := 'TODO(ANDROID)';
{$ENDIF}
{$IFDEF MSWINDOWS}
  DeviceInfoRec.OSNameStr := TPJOSInfo.ProductName;
  DeviceInfoRec.OSVersionStr:= intTostr(TPJOSInfo.MajorVersion)+ '.'
  + intTostr(TPJOSInfo.MinorVersion)+ '.'
  + intTostr(TPJOSInfo.BuildNumber)+ '.';
  DeviceInfoRec.DeviceTypeStr:=intTostr(Win32ProductType);

  DeviceInfoRec.ComputerNameStr := TPJComputerInfo.ComputerName;
  DeviceInfoRec.BoardNameStr := TPJComputerInfo.SystemProductName;
{$ENDIF}
  result:= DeviceInfoRec;
end;


{$IFDEF ANDROID}
function GetCodename(VerString: string): string;
begin
  if VerString = '1.0' then
    result := 'BASE'
  else if VerString = '1.1' then
    result := 'BASE_1_1'
  else if VerString = '1.5' then
    result := 'CUPCAKE'
  else if VerString = '1.6' then
    result := 'DONUT'
  else if VerString = '2.0' then
    result := 'ECLAIR'
  else if VerString = '2.0.1' then
    result := 'ECLAIR_0_1'
  else if VerString = '2.1' then
    result := 'ECLAIR_MR1'
  else if VerString = '2.2' then
    result := 'FROYO'
  else if VerString = '2.3' then
    result := 'GINGERBREAD'
  else if VerString = '2.3.3' then
    result := 'GINGERBREAD_MR1'
  else if VerString = '3.0' then
    result := 'HONEYCOMB'
  else if VerString = '3.1' then
    result := 'HONEYCOMB_MR1'
  else if VerString = '3.2' then
    result := 'HONEYCOMB_MR2'
  else if VerString = '4.0' then
    result := 'ICE_CREAM_SANDWICH'
  else if VerString = '4.0.3' then
    result := 'ICE_CREAM_SANDWICH_MR1'
  else if VerString = '4.1' then
    result := 'JELLY_BEAN'
  else if VerString = '4.2' then
    result := 'JELLY_BEAN_MR1'
  else if VerString = '4.3' then
    result := 'JELLY_BEAN_MR2'
  else if Pos('4.4', VerString) = 1 then
    result := 'KITKAT'
  else
    result := 'UNKNOWN';
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
procedure RetrieveLocalAdapterInformation(strings: TStrings);
var
  pAdapterInfo, pTempAdapterInfo: PIP_ADAPTER_INFO;
  AdapterInfo: IP_ADAPTER_INFO;
  BufLen: DWORD;
  Status: DWORD;
  strMAC: String;
  i: Integer;
begin
  strings.Clear;

  BufLen:= sizeof(AdapterInfo);
  pAdapterInfo:= @AdapterInfo;

  Status:= GetAdaptersInfo(nil, BufLen);
  pAdapterInfo:= AllocMem(BufLen);
  try
    Status:= GetAdaptersInfo(pAdapterInfo, BufLen);

    if (Status <> ERROR_SUCCESS) then
      begin
        case Status of
          ERROR_NOT_SUPPORTED:
            strings.Add('GetAdaptersInfo is not supported by the operating ' +
                        'system running on the local computer.');
          ERROR_NO_DATA:
            strings.Add('No network adapter on the local computer.');
        else
            strings.Add('GetAdaptersInfo failed with error #' + IntToStr(Status));
        end;
        Dispose(pAdapterInfo);
        Exit;
      end;

    while (pAdapterInfo <> nil) do
      begin
        strings.Add('Description: ' + pAdapterInfo^.Description);
        strings.Add('Name: ' + pAdapterInfo^.AdapterName);

        strMAC := '';
        for I := 0 to pAdapterInfo^.AddressLength - 1 do
            strMAC := strMAC + '-' + IntToHex(pAdapterInfo^.Address[I], 2);

        Delete(strMAC, 1, 1);
        strings.Add('MAC address: ' + strMAC);
        strings.Add('IP address: ' + pAdapterInfo^.IpAddressList.IpAddress.S);
        strings.Add('IP subnet mask: ' + pAdapterInfo^.IpAddressList.IpMask.S);
        strings.Add('Gateway: ' + pAdapterInfo^.GatewayList.IpAddress.S);
        strings.Add('DHCP enabled: ' + IntTOStr(pAdapterInfo^.DhcpEnabled));
        strings.Add('DHCP: ' + pAdapterInfo^.DhcpServer.IpAddress.S);
        strings.Add('Have WINS: ' + BoolToStr(pAdapterInfo^.HaveWins,True));
        strings.Add('Primary WINS: ' + pAdapterInfo^.PrimaryWinsServer.IpAddress.S);
        strings.Add('Secondary WINS: ' + pAdapterInfo^.SecondaryWinsServer.IpAddress.S);

        pTempAdapterInfo := pAdapterInfo;
        pAdapterInfo:= pAdapterInfo^.Next;
      if assigned(pAdapterInfo) then Dispose(pTempAdapterInfo);
    end;
  finally
    Dispose(pAdapterInfo);
  end;
end;
{$ENDIF}

end.
