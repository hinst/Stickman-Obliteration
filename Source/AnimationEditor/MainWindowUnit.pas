unit MainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,
  zglHeader,
  LoggerUnit,
  CommonLog, CommonLang, MainMenuUnit, StickStructureUnit, StickStructureJsonLoaderUnit;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MenuButton: TBitBtn;
    ControlsGroupBox: TGroupBox;
    ViewPanel: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuButtonClick(Sender: TObject);
  private
  protected
    FFirstTimeActivation: Boolean;
    FLog: TLogger;
    FEngineModeActive: Boolean;
    FMenuMenu: TAnimationEditorMainMenu;
    procedure SetupMenu;
    procedure FetchEngine;
    procedure TestLoadStickmanStructure(aSender: TObject);
    procedure TestThrowStickmanStructure(aSender: TObject);
  public
    property FirstTimeActivation: Boolean read FFirstTimeActivation;
    property Log: TLogger read FLog;
    property EngineModeActive: Boolean read FEngineModeActive;
    property MenuMenu: TAnimationEditorMainMenu read FMenuMenu;
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

procedure LoadEngineHandler;
var
  panel: TPanel;
begin
  GetGlobalLog.Write('Now loading engine to the Main Window...');
  scr_SetVSync(True);
  panel := MainWindow.ViewPanel;
  wnd_SetPos(panel.Left, panel.Top);
  wnd_SetSize(panel.Width, panel.Height);
  MainWindow.BringToFront;
  MainWindow.FEngineModeActive := True;
end;

procedure DrawBackground;
const
  BackgroundGap = 2;
begin
  pr2d_Rect(
    BackgroundGap, BackgroundGap,
    PtrUInt(zgl_Get(WINDOW_WIDTH)) - 2 * BackgroundGap,
    PtrUInt(zgl_Get(WINDOW_HEIGHT)) - 2 * BackgroundGap,
    $FFFFFF, 255, PR2D_FILL
  );
end;

procedure EngineDrawHandler;
begin
  DrawBackground;
  Application.ProcessMessages;
end;

{ TMainWindow }

procedure TMainWindow.FormActivate(Sender: TObject);
begin
  if
    FirstTimeActivation
  then
  begin
    FFirstTimeActivation := False;
    FetchEngine;
  end;
end;

procedure TMainWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if
    EngineModeActive
  then
  begin
    Log.Write('Close: exiting engine...');
    zgl_Exit;
  end;
end;

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  FFirstTimeActivation := True;
  FLog := TLogger.Create(GetGlobalLog);
  Log.Name := 'MainWindow';
  FEngineModeActive := False;
  SetupMenu;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  Log.Free;
end;

procedure TMainWindow.MenuButtonClick(Sender: TObject);
begin
  MenuMenu.PopUp;
end;

procedure TMainWindow.SetupMenu;
begin
  MenuButton.Caption := Lang['Menu'];
  FMenuMenu := TAnimationEditorMainMenu.Create(self);
  MenuMenu.PopupComponent := MenuButton;
  MenuMenu.LoadStickmanStructure.OnClick := @TestLoadStickmanStructure;
end;

procedure TMainWindow.FetchEngine;
begin
  Log.Write('Now fetching engine...');
  if
    not zglLoad(libZenGL)
  then
    Log.Write('Error', 'Can not load dynamic library...');
  zgl_Reg(SYS_LOAD, @LoadEngineHandler);
  zgl_Reg(SYS_DRAW, @EngineDrawHandler);
  zgl_InitToHandle(ViewPanel.Handle);
end;

procedure TMainWindow.TestLoadStickmanStructure(aSender: TObject);
const
  FileName = '..\Source\Animation\StickmanStructure.json';
var
  structure: TStickStructure;
begin
  Log.Write('Now loading stickman structure from file: "' + FileName + '"');
  structure := CreateStickStructureFromJsonFile(FileName);
  Log.Write(structure.ToDebugText);
  structure.Free;
end;

procedure TMainWindow.TestThrowStickmanStructure(aSender: TObject);
begin

end;

end.

