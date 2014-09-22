unit TreeViewPingItem;

interface
uses FMX.TreeView, FMX.Objects, FMX.Types, Classes;

type
  TTreeViewPingReportItem = class(TTreeViewItem)
  private
    FPingResult: boolean;  //1: ping result rsEcho, 0: other ping result
    FDateTime: string;     //TDateTime;
    FHostName: string;
    FIPAddress: string;
    FStatus: string;
    FImage: TImage;
    FShowImage: Boolean;
    FImageStyleLookup: String;
    procedure SetPingResult(const Value: Boolean);
    procedure SetDateTime(Value: string);
    procedure SetHostName(Value: string);
    procedure SetIPAddress(Value: string);
    procedure SetStatus(Value: string);
    procedure SetShowImage(const Value: Boolean);
    procedure SetImageStyleLookup(const Value: String);
  protected
    procedure ApplyStyle;override;
    procedure FreeStyle;override;
  public
    constructor Create(AOwner: TComponent);override;
  published
    property PingResult: Boolean read FPingResult write SetPingResult default false;
    property DateTime: string read FDateTime write SetDateTime ;
    property HostName: string read FHostName write SetHostName ;
    property IPAddress: string read FIPAddress write SetIPAddress ;
    property Status: string read FStatus write SetStatus ;
    property ImageStyleLookup: String read FImageStyleLookup write SetImageStyleLookup;
    property ShowImage: Boolean read FShowImage write SetShowImage default True;
  end;

implementation

{ TTreeViewImageItem }

procedure TTreeViewPingReportItem.ApplyStyle;
var O: TFMXObject;
begin
  inherited;

  O := FindStyleResource('image');
  if O is TImage then
  begin
    FImage := TImage(O);
    FImage.Visible := ShowImage;
    FImage.Bitmap.StyleLookup := FImageStyleLookup;
  end;
end;

constructor TTreeViewPingReportItem.Create(AOwner: TComponent);
begin
  inherited;
  ShowImage := True;
end;

procedure TTreeViewPingReportItem.FreeStyle;
begin
  inherited;
  FImage := nil;
end;

procedure TTreeViewPingReportItem.SetImageStyleLookup(const Value: String);
//var O: TFMXObject;
begin
  FImageStyleLookup := Value;
  if Assigned(FImage) then
    FImage.Bitmap.StyleLookup := Value;
end;

procedure TTreeViewPingReportItem.SetPingResult(const Value: Boolean);
begin
  FPingResult:= Value;
end;
procedure TTreeViewPingReportItem.SetDateTime(Value: string);
begin
  FDateTime:= Value;
end;
procedure TTreeViewPingReportItem.SetHostName(Value: string);
begin
  FHostName:= Value;
end;
procedure TTreeViewPingReportItem.SetIPAddress(Value: string);
begin
  FIPAddress:= Value;
end;
procedure TTreeViewPingReportItem.SetStatus(Value: string);
begin
  FStatus:= Value;
end;

procedure TTreeViewPingReportItem.SetShowImage(const Value: Boolean);
begin
  FShowImage := Value;
  if Assigned(FImage) then
    FImage.Visible := Value;
end;

end.
