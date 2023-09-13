unit Lazy.Passwords;

interface

uses
  Lazy.Types, System.Classes;

type
  TPasswords = class(TLazyObject)
  private
  public
    class function Generate(ALength: integer; AUpperCase: boolean = true;
      ALowerCase: boolean = true; ADigits: boolean = true;
      AAllowedSpecialCharacters: string = ''): string;
  public

  end;

implementation

uses
  Lazy.Utils;

const
  DefaultAllowedSpecialCharacters = '!@#$-.+';

{ TLazyPasswords }

class function TPasswords.Generate(ALength: integer;
  AUpperCase, ALowerCase, ADigits: boolean;
  AAllowedSpecialCharacters: string): string;
var
  LCharIdx: integer;
  LAllowedSpecialCharacters: string;
  LNumericCharacters, LUpperCaseCharacters, LLowerCaseCharacters: string;
begin
  Result := '';

  LAllowedSpecialCharacters := AAllowedSpecialCharacters;
  if TLazyString.IsEmptyString(LAllowedSpecialCharacters) then
    LAllowedSpecialCharacters := DefaultAllowedSpecialCharacters;

  LNumericCharacters := '';
  LUpperCaseCharacters := '';
  LLowerCaseCharacters := '';
  Randomize;

  // Numeric
  For LCharIdx := 48 to 57 do
  begin
    LNumericCharacters := LNumericCharacters + char(LCharIdx);
  end;

  // Upper
  For LCharIdx := 65 to 90 do
  begin
    LUpperCaseCharacters := LUpperCaseCharacters + char(LCharIdx);
  end;

  // Lower
  For LCharIdx := 97 to 122 do
  begin
    LLowerCaseCharacters := LLowerCaseCharacters + char(LCharIdx);
  end;

  While Length(Result) < ALength do
  begin
    case Random(4) of
      0:
        begin
          if ADigits then
          begin
            Result := Result + LNumericCharacters
              [Random(Length(LNumericCharacters)) + 1];
          end;
        end;
      1:
        begin
          if AUpperCase then
          begin
            Result := Result + LUpperCaseCharacters
              [Random(Length(LUpperCaseCharacters)) + 1];
          end;
        end;
      2:
        begin
          if ALowerCase then
          begin
            Result := Result + LLowerCaseCharacters
              [Random(Length(LLowerCaseCharacters)) + 1];
          end;
        end;
    else
      begin
        if Length(LAllowedSpecialCharacters) > 0 then
        begin
          Result := Result + LAllowedSpecialCharacters
            [Random(Length(LAllowedSpecialCharacters)) + 1];
        end;
      end;
    end;

  end;

end;

end.
