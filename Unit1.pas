unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Xml.xmldom,
  Xml.XMLIntf, Xml.XMLDoc, uCiaXml,  Xml.adomxmldom, FMX.StdCtrls, FMX.Ani,
  FMX.Objects;

type
  TfmxForm = class(TForm)
    XMLConfig1: TXMLConfig;
    ToolBar1: TToolBar;
    sbMenu: TSpeedButton;
    rcPopup: TRectangle;
    PopupAnimation: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure sbMenuClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowPopup;
    procedure HidePopup;
  end;

var
  fmxForm: TfmxForm;

implementation

{$R *.fmx}

procedure TfmxForm.FormCreate(Sender: TObject);
begin
  rcPopup.Position.Y := - rcPopup.Height;
end;

procedure TfmxForm.ShowPopup;
begin
  if (rcPopup.Position.Y = -rcPopup.Height) then begin
    PopupAnimation.StartValue:= -rcPopup.Height;
    PopupAnimation.StopValue:= ToolBar1.Height;
    PopupAnimation.Start;
  end;
end;

procedure TfmxForm.HidePopup;
begin
  if (rcPopup.Position.Y = ToolBar1.Height) then begin
    PopupAnimation.StartValue:= ToolBar1.Height;
    PopupAnimation.StopValue:= -rcPopup.Height;
    PopupAnimation.Start;
  end;
end;

procedure TfmxForm.sbMenuClick(Sender: TObject);
begin
  if (rcPopup.Position.Y = -rcPopup.Height) then ShowPopup
  else HidePopup;
end;

end.
