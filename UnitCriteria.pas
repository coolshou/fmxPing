unit UnitCriteria;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Objects;

type
  TFrameCriteria = class(TFrame)
    rcOption: TRectangle;
    rcOptionLabel: TRectangle;
    lbPingLostNumumber: TLabel;
    lbCriteria: TLabel;
    rcOptionValue: TRectangle;
    spPingLostNumumber: TSpinBox;
    spCriteria: TSpinBox;
    rcButton: TRectangle;
    btnCancel: TButton;
    btnOK: TButton;
    BackGroundRect: TRectangle;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure BackGroundRectClick(Sender: TObject);
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

procedure TFrameCriteria.BackGroundRectClick(Sender: TObject);
begin
    loadOption;
   self.Visible:=false;
end;

procedure TFrameCriteria.btnCancelClick(Sender: TObject);
begin
    loadOption;
   self.Visible:=false;
end;

procedure TFrameCriteria.btnOKClick(Sender: TObject);
begin
   saveOption;
   self.Visible:=false;
end;

constructor TFrameCriteria.Create(AOwner: TComponent);
begin
  inherited;
    loadOption;
end;

procedure TFrameCriteria.loadOption();
begin
  with fmxForm.CfgPingCriteriaRec do
  begin
    spCriteria.Text := sPingLostCriteria;
    spPingLostNumumber.Text := sPingLostNumumber;
  end;

end;

procedure TFrameCriteria.saveOption();
begin
  with fmxForm.CfgPingCriteriaRec do
  begin
    sPingLostCriteria := spCriteria.Text;
    sPingLostNumumber := spPingLostNumumber.Text;
  end;
  fmxForm.setModify(true);

end;

end.
