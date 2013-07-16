unit MainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MenuButton: TBitBtn;
    ControlsGroupBox: TGroupBox;
    ViewPanel: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

end.

