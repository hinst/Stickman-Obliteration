unit MainUnit;

interface

uses
  Classes, SysUtils,
  fpg_main, fpg_form, fpg_panel,
  zglHeader;

type

  { TMainWindow }

  TMainWindow = class(TfpgForm)
  protected
    FViewPanel: TfpgPanel;
    FButtonsPanel: TfpgPanel;
    procedure SetupButtonsPanel;
  public
    property ViewPanel: TfpgPanel read FViewPanel;
    property ButtonsPanel: TfpgPanel read FButtonsPanel;
    procedure AfterCreate; override;
  end;

var
  MainWindow: TMainWindow;

procedure MainRoutine;

procedure FetchZenGLEngine;

implementation

{ TMainWindow }

procedure TMainWindow.SetupButtonsPanel;
begin
  FButtonsPanel := TfpgPanel.Create(self);
  ButtonsPanel.Align := alBottom;
  ButtonsPanel.Height := 24;
end;

procedure TMainWindow.AfterCreate;
begin
  FViewPanel := TfpgPanel.Create(self);
  ViewPanel.Align := alClient;
  SetupButtonsPanel;
end;

procedure MainRoutine;
begin
  fpgApplication.Initialize;
  MainWindow := TMainWindow.Create(nil);
  MainWindow.Width := 800;
  MainWindow.Height := 600;
  MainWindow.Show;
  fpgApplication.Run;
  MainWindow.Free;
end;

procedure FetchZenGLEngine;
begin

end;

end.

