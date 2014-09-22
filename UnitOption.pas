unit UnitOption;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Objects;

type
  TFrameOption = class(TFrame)
    rcOption: TRectangle;
    rcOptionLabel: TRectangle;
    cbPeriodically: TCheckBox;
    lbDelay: TLabel;
    lbPacketSize: TLabel;
    lbPingNumber: TLabel;
    lbTimeoutTime: TLabel;
    lbTTL: TLabel;
    rcOptionValue: TRectangle;
    spDelay: TSpinBox;
    spInterval: TSpinBox;
    spPacketSize: TSpinBox;
    spPingNumber: TSpinBox;
    spTimeoutTime: TSpinBox;
    spTTL: TSpinBox;
    rcButton: TRectangle;
    btnCancel: TButton;
    btnOK: TButton;
    BackGroundRect: TRectangle;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure BackGroundRectClick(Sender: TObject);
    procedure cbPeriodicallyChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure loadOption();
    procedure saveOption();
  end;

implementation

{$R *.fmx}

uses UntMain;

procedure TFrameOption.BackGroundRectClick(Sender: TObject);
begin
    loadOption;
   self.Visible:=false;
end;

procedure TFrameOption.btnCancelClick(Sender: TObject);
begin
    loadOption;
   self.Visible:=false;
end;

procedure TFrameOption.btnOKClick(Sender: TObject);
begin
   saveOption;
   self.Visible:=false;
end;

procedure TFrameOption.cbPeriodicallyChange(Sender: TObject);
begin
    spInterval.Enabled:=cbPeriodically.IsChecked;
end;

constructor TFrameOption.Create(AOwner: TComponent);
begin
  inherited;
    loadOption;
end;

procedure TFrameOption.loadOption();
begin
  with fmxForm.CfgPingRec do
  begin
    cbPeriodically.IsChecked := bPeriodically;
    spInterval.Text := sInterval;
    spTimeoutTime.Text := sTimeoutTime;
    spPacketSize.Text := sPacketSize;
    spPingNumber.Text := sPingNumber;
    spDelay.Text := sDelay;
    spTTL.Text := sTTL;
  end;

end;

procedure TFrameOption.saveOption();
begin
  with fmxForm.CfgPingRec do
  begin
    bPeriodically := cbPeriodically.IsChecked;
    sInterval := spInterval.Text;
    sTimeoutTime := spTimeoutTime.Text;
    sPacketSize := spPacketSize.Text;
    sPingNumber := spPingNumber.Text;
    sDelay := spDelay.Text;
    sTTL := spTTL.Text;
  end;
  fmxForm.setModify(true);

end;

end.
