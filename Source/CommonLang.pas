unit CommonLang;

interface

uses
  Windows, SysUtils, Classes,
  LanguageUnit, LanguageSetUnit, LanguageSetLoaderJsonUnit,
  CommonLog;

var
  GlobalLanguageSet: TLanguageSet;
  GlobalLanguage: TLanguage;

procedure LoadGlobalLanguage;
function Lang: TLanguage;
procedure ReleaseGlobalLanguage;

implementation

procedure LoadGlobalLanguage;
begin
  GetGlobalLog.Write('Now loading language...');
  GlobalLanguageSet := TLanguageSetLoaderJson.LoadFromResource('ApplicationLanguage', RT_RCDATA);
  GlobalLanguage := GlobalLanguageSet.Languages[0];
end;

function Lang: TLanguage;
begin
  result := GlobalLanguage;
end;

procedure ReleaseGlobalLanguage;
begin
  GetGlobalLog.Write('Now releasing language...');
  GlobalLanguageSet.Free;
end;

end.

