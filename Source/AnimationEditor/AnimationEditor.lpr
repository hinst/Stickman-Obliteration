program AnimationEditor;

uses
  Interfaces, Forms, MainWindowUnit;

var
  MainWindow: TMainWindow;

begin
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
end.

