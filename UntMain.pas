unit UntMain;

interface

uses
  System.IOUtils, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Ani,
  FMX.Objects, FMX.Gestures, FMX.ActnList, FMX.TabControl,
  FMX.StdActns, FMX.TreeView, FMX.Layouts, FMX.Memo,
  Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc, uCiaXml, Xml.adomxmldom,
  IdIcmpClient, IdIPAddress,
  DelphiVault.Indy10.PingClient, uDialogs, FMX.Effects, FMX.Filter.Effects,
{$IFDEF ANDROID}
  FMX.Helpers.Android,
{$ENDIF}
  FMX.Edit, System.Sensors, System.Sensors.Components,
  UnitOption;

type
  TCfgPingRec = record
    bPeriodically: Boolean;
    sInterval: string;
    sTimeoutTime: string;
    sPacketSize: string;
    sPingNumber: string;
    sDelay: string;
    sTTL: string;
  end;

  TCfgPingCriteriaRec = record
    sPingLostCriteria: string;
    sPingLostNumumber: string;
  end;

  TCfgLogRec = record
    bEnableLog: Boolean;
    sLogfileName: String;
    bEnableSubffix: Boolean;
    sSubffixString: String;
  end;

  { configFile.WriteBoolean('LOG', 'enableLog', cbEnableLogFile.Checked);
    configFile.WriteString('LOG', 'LogfileName', edLogfileName.Text);
    configFile.WriteBoolean('LOG', 'enableSubffix', cbEnableSubffix.Checked);
    configFile.WriteString('LOG', 'SubffixString', edSubffixString.Text);
  }
  // TODO: let mypingthread keep ping at specify interval
  TMyPingThread = class(TThreadedPing)
  protected
    // thread id?
    procedure SynchronizedResponse(const ReplyStatus: TReplyStatus); override;
  end;

  TfmxForm = class(TForm)
    configFile: TXMLConfig;
    ToolBar1: TToolBar;
    sbMenu: TSpeedButton;
    rcPopup: TRectangle;
    PopupAnimation: TFloatAnimation;
    ActionList1: TActionList;
    GestureManager1: TGestureManager;
    FileExit: TFileExit;
    ActOptions: TAction;
    TabControl1: TTabControl;
    tbItmHosts: TTabItem;
    tbItmReport: TTabItem;
    tbItmStatic: TTabItem;
    PingList: TMemo;
    tvReport: TTreeView;
    tvStatic: TTreeView;
    StyleBook1: TStyleBook;
    PingTimer: TTimer;
    MemoReport: TMemo;
    DEFAULT_CFG: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ActStart: TAction;
    ActStop: TAction;
    ActClear: TAction;
    lbOption: TLabel;
    ShadowEffect1: TShadowEffect;
    lbExit: TLabel;
    Image1: TImage;
    Image2: TImage;
    OrientationSensor1: TOrientationSensor;
    ActShowMenu: TAction;
    Rectangle1: TRectangle;
    ActCriteria: TAction;
    ActLog: TAction;
    lbLog: TLabel;
    lbCriteria: TLabel;
    Line1: TLine;
    ActRunAsServer: TAction;
    lbRunAsServer: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure sbMenuClick(Sender: TObject);
    procedure ConfigLoadExecute(Sender: TObject);
    procedure ConfigSaveExecute(Sender: TObject);
    procedure PingListChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActStartExecute(Sender: TObject);
    procedure ActStopExecute(Sender: TObject);
    procedure ActClearExecute(Sender: TObject);
    procedure ActOptionsExecute(Sender: TObject);
    procedure ActShowMenuExecute(Sender: TObject);
    procedure lbOptionClick(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure lbExitClick(Sender: TObject);
  private
    { Private declarations }
    FCanClose: Boolean;
    FIsModify: Boolean;
    pingListObj: TStringList;
    FrameOption: TFrameOption;
    procedure DoApplyPingResponseItemStyleLookup(Sender: TObject);
    procedure setStartStopTag(bStart: Boolean);
  public
    { Public declarations }
    CfgPingRec: TCfgPingRec;
    CfgPingCriteriaRec: TCfgPingCriteriaRec;
    CfgLogRec: TCfgLogRec;
    procedure ShowPopup;
    procedure HidePopup;
    function getPingList(): string;
    procedure setPingList(s: string);
    procedure AddPingResponse(ReplyStatus: TReplyStatus);
    procedure setModify(bModify: Boolean);
  end;

var
  fmxForm: TfmxForm;

const
  myDateTimeString = 'YYYYMMDD_hh:mm:ss.zzz';
  // pingResponseNG = 'seq %d: %s: %s [%d]';
  // pingResponseOK = 'seq %d: Reply from %s: bytes=%d TTL=%d [%d]';
  pingResponseNG = ' ERROR: %s [%d]';
  pingResponseOK = ' bytes=%d TTL=%d Time=%d [%d]';

implementation

{$R *.fmx}

uses TreeViewPingItem, UnitFuncs, UntConst;

// this procedure only work on  fmPingMain
// TODO: add parent Tform for messageDlg
procedure DoAfterCloseQuery(const MR: TModalResult; dlg: TObject);
begin
  case MR of
    mrYes:
      begin
        fmxForm.ConfigSaveExecute(fmxForm);
      end;
    // mrNo : Form1.Memo1.Lines.Add('¿ï¾Ü:§_');
  end;
  fmxForm.FCanClose := true;
  TMyCustomDialog(dlg).Free;
  fmxForm.Close;
end;

// ==============================================================================
procedure TMyPingThread.SynchronizedResponse(const ReplyStatus: TReplyStatus);
begin
  // Example2: Do something special with ReplyStatus. Here, we're just displaying
  fmxForm.AddPingResponse(ReplyStatus);
end;

// ==============================================================================

procedure TfmxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  pingListObj.Free;
  if assigned(FrameOption) then
    FrameOption.Free;

end;

procedure TfmxForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;
  if FIsModify then
  begin
    // custom info message to save setting
    uDialogs.msg_YesNo(self, 'Save setting before exit?', DoAfterCloseQuery);
  end;

end;

procedure TfmxForm.FormCreate(Sender: TObject);
var
  cfgFileName: string;
  fs: TFileStream;
  defCfgFile: string;
  iWidth, iHeight: integer;
begin
  rcPopup.Position.Y := -rcPopup.Height;
  // PingList.ShowScrollBars:=true;
{$IFDEF DEBUG}
  // for DEBUG MemoryLeaks
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  // Windows need additional folder
  if (TOSVersion.Platform = pfWindows) then
  begin
    cfgFileName := System.IOUtils.TPath.GetHomePath +
      System.IOUtils.TPath.DirectorySeparatorChar + APP_NAME;
    if not System.SysUtils.DirectoryExists(cfgFileName) then
    begin
      if not System.SysUtils.CreateDir(cfgFileName) then
      begin
        raise Exception.Create('File "' + cfgFileName + '" create Fail !');
        exit;
      end;
    end;
  end
  else
  begin
    cfgFileName := System.IOUtils.TPath.GetHomePath;
  end;
  cfgFileName := cfgFileName + System.IOUtils.TPath.DirectorySeparatorChar +
    APP_NAME + '_cfg.xml';

  if not FileExists(cfgFileName) then
  begin
    try
      fs := System.IOUtils.TFile.Create(cfgFileName);
    except
      on E: Exception do
      begin
        raise Exception.Create('File "' + cfgFileName + '" create Fail !');
        exit;
      end;
    end;
    fs.Position := 0;
    // default xml config store in a invisiable Tmemo
    DEFAULT_CFG.Lines.SaveToStream(fs);
    fs.Free;
  end;
  configFile.FileName := cfgFileName;
  // make sure file existed
  configFile.Active := true;

  PingList.OnChangeTracking := nil;
  // load config
  ConfigLoadExecute(self);
  PingList.OnChangeTracking := PingListChange;

  pingListObj := TStringList.Create;
  // tvReport.ShowScrollBars := true;
  PingTimer.Enabled := false;

  // show hosts list tab
  TabControl1.TabIndex := 0;

  // change  windows Orientation
{$IFDEF MSWINDOWS}
  // Horizontal
  iWidth := fmxForm.Width;
  iHeight := fmxForm.Height;
  fmxForm.Height := iWidth;
  fmxForm.Width := iHeight;
{$ENDIF}
{$IFDEF ANDROID}
  {
    if  SharedActivity.getRequestedOrientation  = 1 then
    SharedActivity.setRequestedOrientation(0)
    else if SharedActivity.getRequestedOrientation = 0 then
    SharedActivity.setRequestedOrientation(1);
  }
{$ENDIF}
end;

procedure TfmxForm.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiRotate then
  begin
    // TODO:  rotate the form?
{$IFDEF ANDROID}
    if SharedActivity.getRequestedOrientation = 1 then
      SharedActivity.setRequestedOrientation(0)
    else if SharedActivity.getRequestedOrientation = 0 then
      SharedActivity.setRequestedOrientation(1);
{$ENDIF}
  end;

end;

procedure TfmxForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  // any key close option
  if assigned(FrameOption) then
    FrameOption.Visible := false;

end;

procedure TfmxForm.PingListChange(Sender: TObject);
begin
  setModify(true);
  // TODO: check input is valid IPv4 or IPv6 address
  // Ignore #
  setPingList(PingList.Text);

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

procedure TfmxForm.ShowPopup;
begin
  if (rcPopup.Position.Y = -rcPopup.Height) then
  begin
    PopupAnimation.StartValue := -rcPopup.Height;
    PopupAnimation.StopValue := ToolBar1.Height;
    PopupAnimation.Start;
  end;
end;

procedure TfmxForm.HidePopup;
begin
  if (rcPopup.Position.Y = ToolBar1.Height) then
  begin
    PopupAnimation.StartValue := ToolBar1.Height;
    PopupAnimation.StopValue := -rcPopup.Height;
    PopupAnimation.Start;
  end;
end;

procedure TfmxForm.lbExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmxForm.lbOptionClick(Sender: TObject);
begin
  ActOptions.Execute;
end;

procedure TfmxForm.sbMenuClick(Sender: TObject);
begin
  if (rcPopup.Position.Y = -rcPopup.Height) then
    ShowPopup
  else
    HidePopup;
end;

procedure TfmxForm.ConfigLoadExecute(Sender: TObject);
begin
  // ping
  // edtHost.Text := configFile.ReadString('PING', 'host', '192.168.0.1');
  setPingList(configFile.ReadString('PING', 'hosts', '192.168.0.1'));
  CfgPingRec.bPeriodically := configFile.ReadBoolean('PING',
    'Periodically', true);
  CfgPingRec.sInterval := configFile.ReadString('PING', 'Interval', '1');
  CfgPingRec.sTimeoutTime := configFile.ReadString('PING',
    'TimeoutTime', '2000');
  CfgPingRec.sPacketSize := configFile.ReadString('PING', 'PacketSize', '1024');
  CfgPingRec.sPingNumber := configFile.ReadString('PING', 'PingNumber', '5');
  CfgPingRec.sDelay := configFile.ReadString('PING', 'Delay', '2000');
  CfgPingRec.sTTL := configFile.ReadString('PING', 'TTL', '255');
  // Criteria
  CfgPingCriteriaRec.sPingLostCriteria := configFile.ReadString('Criteria',
    'PingLostCriteria', '5');
  CfgPingCriteriaRec.sPingLostNumumber := configFile.ReadString('Criteria',
    'PingLostNumumber', '10');
  // log
  CfgLogRec.bEnableLog := configFile.ReadBoolean('LOG', 'enableLog', true);
  CfgLogRec.sLogfileName := configFile.ReadString('LOG', 'LogFileName',
    'test.log');
  CfgLogRec.bEnableSubffix := configFile.ReadBoolean('LOG',
    'enableSubffix', false);
  CfgLogRec.sSubffixString := configFile.ReadString('LOG', 'SubffixString',
    'YYYY-MM-DD_hhmmss');

  {
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

procedure TfmxForm.ConfigSaveExecute(Sender: TObject);
begin
  // ping
  // configFile.WriteString('PING', 'host', edtHost.Text);
  // save PingList's all text (include #)
  configFile.WriteString('PING', 'hosts', PingList.Lines.Text);
  //
  configFile.WriteBoolean('PING', 'Periodically', CfgPingRec.bPeriodically);
  configFile.WriteString('PING', 'Interval', CfgPingRec.sInterval);
  configFile.WriteString('PING', 'TimeoutTime', CfgPingRec.sTimeoutTime);
  configFile.WriteString('PING', 'PacketSize', CfgPingRec.sPacketSize);
  configFile.WriteString('PING', 'PingNumber', CfgPingRec.sPingNumber);
  configFile.WriteString('PING', 'Delay', CfgPingRec.sDelay);
  configFile.WriteString('PING', 'TTL', CfgPingRec.sTTL);
  // Criteria
  configFile.WriteString('Criteria', 'PingLostCriteria',
    CfgPingCriteriaRec.sPingLostCriteria);
  configFile.WriteString('Criteria', 'PingLostNumumber',
    CfgPingCriteriaRec.sPingLostNumumber);
  // log
  configFile.WriteBoolean('LOG', 'enableLog', CfgLogRec.bEnableLog);
  configFile.WriteString('LOG', 'LogFileName', CfgLogRec.sLogfileName);
  configFile.WriteBoolean('LOG', 'enableSubffix', CfgLogRec.bEnableSubffix);
  configFile.WriteString('LOG', 'SubffixString', CfgLogRec.sSubffixString);
  {
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
  //

end;

procedure TfmxForm.setPingList(s: string);
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
    AStream.Free;
  end;

end;

// return IP address string in PingList
function TfmxForm.getPingList(): string;
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

procedure TfmxForm.ActClearExecute(Sender: TObject);
begin
  MemoReport.Lines.Clear;
  tvReport.Clear;
end;

procedure TfmxForm.ActOptionsExecute(Sender: TObject);
begin
  //
  HidePopup;
  if not assigned(FrameOption) then
    FrameOption := TFrameOption.Create(self);
  FrameOption.Parent := self;
  FrameOption.Align := TAlignLayout.Center;
  // FrameOption.BringToFront;
  FrameOption.Visible := true;

  // form not work will, try frame
  {
    if not assigned(optionForm) then
    optionForm := TOptionForm.Create(self);
    if (optionForm.ShowModal = mrOk) then
    begin
    setModify(true);
    end;
  }
end;

procedure TfmxForm.ActShowMenuExecute(Sender: TObject);
begin
  if (rcPopup.Position.Y = -rcPopup.Height) then
    ShowPopup
  else
    HidePopup;
end;

procedure TfmxForm.ActStartExecute(Sender: TObject);
var
  I: integer;
  s: string;
  idx: integer;
  ls: TStringList;
  mpt: TMyPingThread;
  bIsIPv4: Boolean;
  bIsIPv6: Boolean;
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
              strToInt(CfgPingRec.sTimeoutTime),
              strToInt(CfgPingRec.sPacketSize), IPToCheck.AddrType);
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
      mpt.setPingNumber(strToInt(CfgPingRec.sPingNumber));
      mpt.setDelay(strToInt(CfgPingRec.sDelay));
      mpt.setTimeout(strToInt(CfgPingRec.sTimeoutTime));
      mpt.setPacketSize(strToInt(CfgPingRec.sPacketSize));
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

procedure TfmxForm.ActStopExecute(Sender: TObject);
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

procedure TfmxForm.AddPingResponse(ReplyStatus: TReplyStatus);
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

procedure TfmxForm.setModify(bModify: Boolean);
begin
  // ignore when already modify
  if (CharInSet(fmxForm.Caption[1], ['*']) and bModify) then
    exit;

  FIsModify := bModify;
  if FIsModify then
  begin
    fmxForm.Caption := 'fmxPing ' + '*';
  end
  else
  begin
    fmxForm.Caption := 'fmxPing';
  end;
  FCanClose := not FIsModify;
end;

procedure TfmxForm.DoApplyPingResponseItemStyleLookup(Sender: TObject);
var
  itm: TTreeViewPingReportItem;
  img: TImage;
  Obj: TFMXObject;
  // imgStr: string;
begin
  itm := TTreeViewPingReportItem(Sender);
  begin
    Obj := itm.FindStyleResource('image');
    if Obj is TImage then
    begin
      img := TImage(Obj);
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

procedure TfmxForm.setStartStopTag(bStart: Boolean);
begin
  ActStart.Enabled := not bStart;
  ActStop.Enabled := bStart;
  ActClear.Enabled := not bStart;

end;

end.
