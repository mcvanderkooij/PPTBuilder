program PPTBuilder;

uses
  Vcl.Forms,
  UfrmPPTView in '..\..\source\UfrmPPTView.pas' {frmPPTViewer},
  PowerPoint_TLB in '..\..\source\PowerPoint_TLB.pas',
  Office_TLB in '..\..\source\Office_TLB.pas',
  VBIDE_TLB in '..\..\source\VBIDE_TLB.pas',
  USlideLayout in '..\..\source\USlideLayout.pas',
  USlide in '..\..\source\USlide.pas',
  USourcePPT in '..\..\source\USourcePPT.pas',
  TeeGenericTree in '..\..\source\TeeGenericTree.pas';

{$R *.res}

begin
  Vcl.Forms.Application.Initialize;
  Vcl.Forms.Application.MainFormOnTaskbar := True;
  Vcl.Forms.Application.CreateForm(TfrmPPTViewer, frmPPTViewer);
  Vcl.Forms.Application.Run;
end.
