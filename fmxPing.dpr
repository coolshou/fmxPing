program fmxPing;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  UntMain in 'UntMain.pas' {fmxForm},
  DelphiVault.Indy10.PingClient in 'lib\DelphiVault.Indy10.PingClient.pas',
  TreeViewPingItem in 'lib\TreeViewPingItem.pas',
  uDialogs in 'lib\uDialogs.pas',
  UnitFuncs in 'UnitFuncs.pas',
  UntConst in 'UntConst.pas',
  {$IFDEF MSWINDOWS}
  PJSysInfo in 'lib\PJSysInfo.pas',
  {$ENDIF }
  UnitCriteria in 'UnitCriteria.pas' {FrameCriteria: TFrame},
  UnitOption in 'UnitOption.pas' {FrameOption: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmxForm, fmxForm);
  Application.Run;
end.
