program AnimationEditor;

uses
  Interfaces,
  Forms,
  CommonLog,
  MainWindowUnit, StickStructureUnit, StickStructureJsonLoaderUnit,
  MainMenuUnit, CommonLang;

procedure WidgetsetApplicationRoutine;
begin
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
end;

{$R *.res}

{$R MainResources.rc}

begin
  StartupGlobalLog;
  LoadGlobalLanguage;
  WidgetsetApplicationRoutine;
  ReleaseGlobalLanguage;
  ShutdownGlobalLog;
end.

