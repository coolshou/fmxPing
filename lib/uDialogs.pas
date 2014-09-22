unit uDialogs;

interface

uses
  System.Classes, System.UITypes, System.SysUtils, FMX.Forms, FMX.Objects,
  FMX.Types, FMX.StdCtrls, FMX.Controls, FMX.Layouts, FMX.Dialogs;

type
  TDlgCallbackProcedure = procedure(const MR: TModalResult; dlg: TObject);

  TMyCustomDialog = class
  private
    FOwnerForm: TCustomForm;
    FBackGroundRectangle: TRectangle;
    FFrontRectangle: TRectangle;
    // FScreenScale : Single;
    // Message Rectangle 上的各種 component
    FMsgTopLayout: TLayout;
    FMsgBottomLayout: TLayout;
    FMsgTitle: TLabel;
    FMsgLineRect: TRectangle;
    FMsgBody: TText;
    FButtonOK: TButton;
    FButtonCancel: TButton;
    FCallback: TDlgCallbackProcedure;

    // FWordDefaultHeight : Single;
    // FWordDefaultWidth : Single;
    procedure DoInitial(Sender: TObject);

    procedure FMsgBottomLayoutResize(Sender: TObject);
    procedure ButtonYesClick(Sender: TObject);
    procedure ButtonNoClick(Sender: TObject);
    procedure SetTitle(const Value: string);
    function GetTitle: string;
    function GetMsg: string;
    procedure SetMsg(const Value: string);
  public
    constructor Create(AForm: TCustomForm);
    destructor Destroy();
    property Title: string read GetTitle write SetTitle;
    property Msg: string read GetMsg write SetMsg;
  end;

procedure MessageDlg(const AForm: TCustomForm; const aTitle, aMsg: string;
  DlgType: TMsgDlgType; ACallback: TDlgCallbackProcedure);

procedure msg_OK(const AForm: TCustomForm; const aMsg: string;
  ACallback: TDlgCallbackProcedure);
procedure msg_Err(const AForm: TCustomForm; const aMsg: string;
  ACallback: TDlgCallbackProcedure);
procedure msg_War(const AForm: TCustomForm; const aMsg: string;
  ACallback: TDlgCallbackProcedure);
procedure msg_YesNo(const AForm: TCustomForm; const aMsg: string;
  ACallback: TDlgCallbackProcedure);

implementation

uses
  FMX.Graphics, System.IOUtils;

var
  gMyCustomDialog: TMyCustomDialog;

{* MessageDlg will create TMyCustomDialog each time.
 In TDlgCallbackProcedure callback need to free TMyCustomDialog
*}
procedure MessageDlg(const AForm: TCustomForm; const aTitle, aMsg: string;
  DlgType: TMsgDlgType; ACallback: TDlgCallbackProcedure);
begin
  AForm.BeginUpdate;
  try
    if gMyCustomDialog = nil then
      gMyCustomDialog := TMyCustomDialog.Create(AForm);

    with gMyCustomDialog do
    begin
      Title := aTitle;
      Msg := aMsg;
      FCallback := ACallback;

      if DlgType = TMsgDlgType.mtConfirmation then
        FButtonCancel.Visible := True
      else
        FButtonCancel.Visible := False;

      FBackGroundRectangle.BringToFront;
      FFrontRectangle.BringToFront;
      FBackGroundRectangle.Visible := True;
      FFrontRectangle.Visible := True;
    end;
  finally
    AForm.EndUpdate;
  end;
end;

procedure msg_OK(const AForm: TCustomForm; const aMsg: string;
  ACallback: TDlgCallbackProcedure);
begin
  MessageDlg(AForm, '訊息', aMsg, TMsgDlgType.mtInformation, ACallback);
end;

procedure msg_Err(const AForm: TCustomForm; const aMsg: string;
  ACallback: TDlgCallbackProcedure);
begin
  MessageDlg(AForm, '錯誤', aMsg, TMsgDlgType.mtError, ACallback);
end;

procedure msg_War(const AForm: TCustomForm; const aMsg: string;
  ACallback: TDlgCallbackProcedure);
begin
  MessageDlg(AForm, '警告', aMsg, TMsgDlgType.mtWarning, ACallback);
end;

procedure msg_YesNo(const AForm: TCustomForm; const aMsg: string;
  ACallback: TDlgCallbackProcedure);
begin
  MessageDlg(AForm, '確認', aMsg, TMsgDlgType.mtConfirmation, ACallback);
end;

{ TMyCustomDialog }

procedure TMyCustomDialog.ButtonNoClick(Sender: TObject);
begin
  FBackGroundRectangle.Visible := False;
  FFrontRectangle.Visible := False;
  if Assigned(FCallback) then
    FCallback(mrNo, gMyCustomDialog);
end;

procedure TMyCustomDialog.ButtonYesClick(Sender: TObject);
begin
  FBackGroundRectangle.Visible := False;
  FFrontRectangle.Visible := False;
  if Assigned(FCallback) then
    FCallback(mrYes, gMyCustomDialog);
end;

constructor TMyCustomDialog.Create(AForm: TCustomForm);
begin
  FOwnerForm := AForm;

  DoInitial(FOwnerForm);
end;

destructor TMyCustomDialog.Destroy();
begin
  if gMyCustomDialog <> nil then
    gMyCustomDialog.Free;
end;

procedure TMyCustomDialog.DoInitial(Sender: TObject);
var
  lScreenWidth: Extended;
begin
  // 設定背景 Rectangle ----- Begin
  FBackGroundRectangle := TRectangle.Create(Tcomponent(Sender));
  with FBackGroundRectangle do
  begin
    Parent := FOwnerForm;
    Visible := False;
    // Align := TAlignLayout.alContents;
    Align := TAlignLayout.Contents;
    Sides := [];
    Opacity := 0.8;
    Fill.Color := TAlphaColorRec.Black;
  end;
  // 設定背景 Rectangle ----- End

  // 設定前景 Rectangle ----- Begin
  FFrontRectangle := TRectangle.Create(FBackGroundRectangle);
  with FFrontRectangle do
  begin
    Parent := FOwnerForm;
    Visible := False;
    // Align := TAlignLayout.alCenter;
    Align := TAlignLayout.Center;
    Height := 214;
    Opacity := 1;

    lScreenWidth := FOwnerForm.Width;

    // Width 小於等於 430，則設定為螢幕寬度
    if lScreenWidth <= 430 then
      Width := lScreenWidth - 40
    else
      Width := 430;

    // Stroke.Kind := TBrushKind.bkNone;
    Stroke.Kind := TBrushKind.None;

    // Fill.Kind := TBrushKind.bkGradient;
    Fill.Kind := TBrushKind.Gradient;
    Fill.Gradient.Color := TAlphaColorRec.White;
  end;
  // 設定前景 Rectangle ----- End

  // 設定前景上各種元件 ----- Begin
  FMsgBody := TText.Create(FFrontRectangle);
  with FMsgBody do
  begin
    Parent := FFrontRectangle;
    // Align := TAlignLayout.alClient;
    Align := TAlignLayout.Client;
    Margins.Top := 5;
    Margins.Bottom := 5;
    Margins.Left := 10;
    Margins.Right := 10;
    Font.Size := 18;
    Text := 'Message';
    // HorzTextAlign := TTextAlign.taLeading;
    HorzTextAlign := TTextAlign.Leading;
    // VertTextAlign := TTextAlign.taLeading;
    VertTextAlign := TTextAlign.Leading;
  end;

  FMsgTopLayout := TLayout.Create(FFrontRectangle);
  with FMsgTopLayout do
  begin
    Parent := FFrontRectangle;
    // Align := TAlignLayout.alTop;
    Align := TAlignLayout.Top;
    Height := 44;
  end;

  FMsgTitle := TLabel.Create(FFrontRectangle);
  with FMsgTitle do
  begin
    Parent := FMsgTopLayout;
    // Align := TAlignLayout.alClient;
    Align := TAlignLayout.Client;
    Margins.Bottom := 5;
    Margins.Left := 5;
    Margins.Right := 10;
    Margins.Top := 5;
    Text := 'Title';
  end;

  FMsgLineRect := TRectangle.Create(FFrontRectangle);
  with FMsgLineRect do
  begin
    Parent := FMsgTopLayout;
    // Align := TAlignLayout.alBottom;
    Align := TAlignLayout.Bottom;
    Height := 2;
    Margins.Left := 10;
    Margins.Right := 10;
    Stroke.Color := TAlphaColorRec.Cornflowerblue;
    Stroke.Thickness := 2;
    // Sides := [TSide.sdBottom];
    Sides := [TSide.Bottom];
    // Stroke.Kind := TBrushKind.bkSolid;
    Stroke.Kind := TBrushKind.Solid;
  end;

  FMsgBottomLayout := TLayout.Create(FFrontRectangle);
  with FMsgBottomLayout do
  begin
    Parent := FFrontRectangle;
    // Align := TAlignLayout.alBottom;
    Align := TAlignLayout.Bottom;
    Height := 44;
    Margins.Bottom := 4;
    Margins.Left := 4;
    Margins.Right := 4;
  end;

  FButtonCancel := TButton.Create(FFrontRectangle);
  with FButtonCancel do
  begin
    Parent := FMsgBottomLayout;
    // Align := TAlignLayout.alRight;
    Align := TAlignLayout.Right;
    Text := '取消';
    OnClick := ButtonNoClick;
  end;

  FButtonOK := TButton.Create(FFrontRectangle);
  with FButtonOK do
  begin
    Parent := FMsgBottomLayout;
    // Align := TAlignLayout.alClient;
    Align := TAlignLayout.Client;
    Text := '確定';
    OnClick := ButtonYesClick;
  end;

  FMsgBottomLayout.OnResize := FMsgBottomLayoutResize;
  FMsgBottomLayoutResize(FFrontRectangle);

  // 設定前景上各種元件 ----- End
end;

procedure TMyCustomDialog.FMsgBottomLayoutResize(Sender: TObject);
begin
  FButtonCancel.Width := FMsgBottomLayout.Width / 2;
end;

function TMyCustomDialog.GetMsg: string;
begin
  result := FMsgBody.Text;
end;

function TMyCustomDialog.GetTitle: string;
begin
  result := FMsgTitle.Text;
end;

procedure TMyCustomDialog.SetMsg(const Value: string);
begin
  FMsgBody.Text := Value;
end;

procedure TMyCustomDialog.SetTitle(const Value: string);
begin
  FMsgTitle.Text := Value;
end;

end.
