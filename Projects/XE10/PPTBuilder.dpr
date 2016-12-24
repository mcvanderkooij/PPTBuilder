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
  TeeGenericTree in '..\..\source\TeeGenericTree.pas',
  UfrmMain in '..\..\source\UfrmMain.pas' {frmMain},
  USlideTemplate in '..\..\source\USlideTemplate.pas',
  UUtilsStrings in '..\..\source\UUtilsStrings.pas',
  UfrmPictureSelector in '..\..\source\UfrmPictureSelector.pas' {frmPictureSelector},
  UUtils in '..\..\source\UUtils.pas',
  UBuildPowerpoint in '..\..\source\UBuildPowerpoint.pas',
  UProject in '..\..\source\UProject.pas',
  UfrmMemoDlg in '..\..\source\UfrmMemoDlg.pas' {frmMemoDlg},
  UUtilsForms in '..\..\source\UUtilsForms.pas',
  gnugettext in '..\..\source\gnugettext.pas',
  UfrmListBoxDlg in '..\..\source\UfrmListBoxDlg.pas' {frmListBoxDlg},
  UFastKeys in '..\..\source\UFastKeys.pas',
  UFastKeysSS in '..\..\source\UFastKeysSS.pas',
  UTempActions in '..\..\source\UTempActions.pas',
  UFastKeysSO in '..\..\source\UFastKeysSO.pas',
  UOverview in '..\..\source\UOverview.pas',
  URibbon in '..\..\source\URibbon.pas',
  UfrmEditExtSlide in '..\..\source\UfrmEditExtSlide.pas' {frmEditExtSlide},
  UfrmEditText in '..\..\source\UfrmEditText.pas' {frmEditText},
  USplitBibleVerses in '..\..\source\USplitBibleVerses.pas',
  UfrmPictureDescription in '..\..\source\UfrmPictureDescription.pas' {frmPictureDescription},
  UfrmEditSong in '..\..\source\UfrmEditSong.pas' {frmEditSong},
  UfrmEditSinglePage in '..\..\source\UfrmEditSinglePage.pas' {frmEditSinglePage},
  UStringLogicalComparer in '..\..\source\UStringLogicalComparer.pas',
  USettings in '..\..\source\USettings.pas',
  skushell in '..\..\source\skushell.pas',
  ULiturgy in '..\..\source\ULiturgy.pas',
  URegexReplaceProperties in '..\..\source\URegexReplaceProperties.pas',
  USourceInfo in '..\..\source\USourceInfo.pas',
  UfrmBrowseFTP in '..\..\source\UfrmBrowseFTP.pas' {frmBrowseFTP},
  USlideVariables in '..\..\source\USlideVariables.pas',
  UfrmEditVariables in '..\..\source\UfrmEditVariables.pas' {frmEditVariables},
  UUtilsFeedback in '..\..\source\UUtilsFeedback.pas',
  UfrmSettings in '..\..\source\UfrmSettings.pas' {frmSettings},
  UUtilsJSON in '..\..\source\UUtilsJSON.pas',
  UfrmSelectString in '..\..\source\UfrmSelectString.pas' {frmSelectString},
  UBrowseFTP in '..\..\source\UBrowseFTP.pas',
  UfrmQuickStart in '..\..\source\UfrmQuickStart.pas' {frmQuickStart},
  UMRUList in '..\..\source\UMRUList.pas',
  USnapshot in '..\..\source\USnapshot.pas',
  UfrmEditBook in '..\..\source\UfrmEditBook.pas' {frmEditBook},
  USourceBook in '..\..\source\USourceBook.pas',
  UfrmBrowseBook in '..\..\source\UfrmBrowseBook.pas' {frmBrowseBook},
  RegExpr in '..\..\source\RegExpr.pas',
  UfrmNewProjectOptions in '..\..\source\UfrmNewProjectOptions.pas' {frmNewProjectOptions},
  UframeProjectProperties in '..\..\source\UframeProjectProperties.pas' {FrameProjectProperties: TFrame};

{$R *.res}

begin
  Vcl.Forms.Application.Initialize;
  Vcl.Forms.Application.MainFormOnTaskbar := True;
  Vcl.Forms.Application.Title := _('Powerpoint Builder');
  Vcl.Forms.Application.CreateForm(TfrmMain, frmMain);
  Vcl.Forms.Application.Run;
end.
