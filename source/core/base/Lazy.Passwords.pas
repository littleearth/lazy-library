unit Lazy.Passwords;

interface

uses
  Lazy.Types, System.Classes;

type
  TLZPasswords = class(TLZObject)
  private
    class var FWordList1: TArray<string>;
    class var FWordList2: TArray<string>;
    class var FWordList3: TArray<string>;
    class procedure InitializeWordLists;
  public
    class constructor Create;
    class function Generate(ALength: integer; AUpperCase: boolean = true;
      ALowerCase: boolean = true; ADigits: boolean = true;
      AAllowedSpecialCharacters: string = ''): string;
    class function GeneratePhrase: string;
    class function HashPassword(const APassword: string): string;
    class function ValidatePasswordHash(const APassword, AHash: string)
      : boolean;
  public

  end;

implementation

uses
  Lazy.Utils, System.SysUtils, System.Hash;

const
  DefaultAllowedSpecialCharacters = '!@#$-.+';

  { TLZPasswords }

class constructor TLZPasswords.Create;
begin
  InitializeWordLists;
end;

class procedure TLZPasswords.InitializeWordLists;
begin
  // Adjectives
  FWordList1 := ['Happy', 'Bright', 'Swift', 'Gentle', 'Bold', 'Clever', 'Calm',
    'Brave', 'Eager', 'Fair', 'Kind', 'Lively', 'Mighty', 'Noble', 'Quick',
    'Quiet', 'Royal', 'Smart', 'Tall', 'Warm', 'Wise', 'Witty', 'Jolly',
    'Merry', 'Lucky', 'Lovely', 'Grand', 'Great', 'Sweet', 'Pure'];

  // Nouns (Nature/Objects)
  FWordList2 := ['Sky', 'Ocean', 'Mountain', 'River', 'Forest', 'Desert',
    'Valley', 'Lake', 'Meadow', 'Cloud', 'Star', 'Moon', 'Sun', 'Storm', 'Rain',
    'Snow', 'Wind', 'Fire', 'Earth', 'Stone', 'Tree', 'Leaf', 'Flower',
    'Garden', 'Bridge', 'Castle', 'Tower', 'Road', 'Path', 'Gate'];

  // Verbs/Actions (plural)
  FWordList3 := ['Dances', 'Sings', 'Runs', 'Flies', 'Jumps', 'Swims', 'Grows',
    'Shines', 'Glows', 'Waves', 'Flows', 'Rises', 'Falls', 'Blooms', 'Speaks',
    'Whispers', 'Echoes', 'Rings', 'Calls', 'Dreams', 'Hopes', 'Thinks',
    'Wonders', 'Travels', 'Wanders', 'Explores', 'Discovers', 'Creates',
    'Builds', 'Protects'];
end;

class function TLZPasswords.ValidatePasswordHash(const APassword,
  AHash: string): boolean;
var
  LCalculatedHash: string;
begin
  LCalculatedHash := HashPassword(APassword);
  Result := SameText(LCalculatedHash, AHash);
end;

class function TLZPasswords.GeneratePhrase: string;
var
  LWord1, LWord2, LWord3: string;
  LNumber: integer;
  LSpecialChars: array [0 .. 5] of Char;
  LSpecialChar: Char;
begin
  Randomize;

  // Pick random words from each list
  LWord1 := FWordList1[Random(Length(FWordList1))];
  LWord2 := FWordList2[Random(Length(FWordList2))];
  LWord3 := FWordList3[Random(Length(FWordList3))];

  // Add a random number (2-3 digits)
  LNumber := Random(900) + 100; // 100-999

  // Optional: Add a special character
  LSpecialChars[0] := '!';
  LSpecialChars[1] := '@';
  LSpecialChars[2] := '#';
  LSpecialChars[3] := '*';
  LSpecialChars[4] := '+';
  LSpecialChars[5] := '=';

  // Randomly include special char (50% chance)
  if Random(2) = 0 then
    LSpecialChar := LSpecialChars[Random(6)]
  else
    LSpecialChar := #0;

  // Combine into password
  if LSpecialChar <> #0 then
    Result := Format('%s%s%s%d%s', [LWord1, LWord2, LWord3, LNumber,
      LSpecialChar])
  else
    Result := Format('%s%s%s%d', [LWord1, LWord2, LWord3, LNumber]);
end;

class function TLZPasswords.HashPassword(const APassword: string): string;
begin
  Result := THashSHA2.GetHashString(APassword);
end;

class function TLZPasswords.Generate(ALength: integer;
  AUpperCase, ALowerCase, ADigits: boolean;
  AAllowedSpecialCharacters: string): string;
var
  LCharIdx: integer;
  LAllowedSpecialCharacters: string;
  LNumericCharacters, LUpperCaseCharacters, LLowerCaseCharacters: string;
begin
  Result := '';

  LAllowedSpecialCharacters := AAllowedSpecialCharacters;
  if TLZString.IsEmptyString(LAllowedSpecialCharacters) then
    LAllowedSpecialCharacters := DefaultAllowedSpecialCharacters;

  LNumericCharacters := '';
  LUpperCaseCharacters := '';
  LLowerCaseCharacters := '';
  Randomize;

  // Numeric
  For LCharIdx := 48 to 57 do
  begin
    LNumericCharacters := LNumericCharacters + Char(LCharIdx);
  end;

  // Upper
  For LCharIdx := 65 to 90 do
  begin
    LUpperCaseCharacters := LUpperCaseCharacters + Char(LCharIdx);
  end;

  // Lower
  For LCharIdx := 97 to 122 do
  begin
    LLowerCaseCharacters := LLowerCaseCharacters + Char(LCharIdx);
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
