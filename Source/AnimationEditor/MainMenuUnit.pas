unit MainMenuUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus,
  CommonLang;

type

  { TAnimationEditorMainMenu }
  TAnimationEditorMainMenu = class(TPopupMenu)
  protected
    procedure Setup;
  public
    Test, LoadStickmanStructure: TMenuItem;
    constructor Create(aOwner: TComponent); override;
  end;

implementation

{ TAnimationEditorMainMenu }

procedure TAnimationEditorMainMenu.Setup;
begin
  Test := TMenuItem.Create(self);
  Test.Caption := Lang['Test'];
  Items.Add(Test);

  LoadStickmanStructure := TMenuItem.Create(self);
  LoadStickmanStructure.Caption := Lang['LoadStickmanStructure'];
  Test.Add(LoadStickmanStructure);
end;

constructor TAnimationEditorMainMenu.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Setup;
end;

end.

