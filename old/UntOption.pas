unit UntOption;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Objects;

type
  TOptionForm = class(TForm)
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure saveOption();
    procedure loadOption();
  end;

var
  OptionForm: TOptionForm;

implementation

{$R *.fmx}

uses UntMain;

procedure TOptionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  saveOption
end;

procedure TOptionForm.saveOption();
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

end;

procedure TOptionForm.FormCreate(Sender: TObject);
begin
  loadOption;

end;

procedure TOptionForm.loadOption();
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

end.
