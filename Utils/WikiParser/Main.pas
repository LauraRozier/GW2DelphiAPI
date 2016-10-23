unit Main;
interface
uses
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, SysUtils, Classes, StdCtrls, StrUtils, INIFiles;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    txtParserOutput: TMemo;
    edtInputFile: TEdit;
    edtOutputFile: TEdit;
    btnGenerate: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure txtParserOutputKeyPress(Sender: TObject; var Key: Char);
    procedure edtOnTextChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fSettingsPath: string;
    fSafeToWrite: Boolean;
    procedure ParseText(aFile: string; aList: TStringList; aHasReturn: Boolean);
    function ParseParams(aString: string; aDescriptions: TStringList): string;
    procedure Reinit;
  end;

  TParamHolder = record
    Name, varType: string;
  end;

  TCommandInfo = record
    Version:     string;
    Group:       string;
    Name:        string;
    Description: string;
    Parameters:  string;
    Return:      string;
    ReturnDesc:  string;
  end;

const
  VAR_TYPE_COUNT = 63;
  VAR_TYPE_NAME: array[0..VAR_TYPE_COUNT-1] of string = (
    // System default types
    'Byte', 'Shortint', 'Smallint', 'Word', 'Integer', 'Cardinal', 'Single',
    'Boolean', 'AnsiString', 'String', 'array of const', 'array of Integer',
    'array of string',
    // Generics types
    'TEnum',
    // GW2DA custom types
    'TAPIVersion', 'TAPIFunction', 'TAPIPermissions', 'TWebHandler',
    'TGW2Token', 'TGW2Version', 'TGW2ColorDetail', 'TGW2Color', 'TGW2Quaggan',
    'TGW2World', 'TGW2Currency', 'TGW2File', 'TGW2Mini', 'TGW2Account',
    'TGW2AccountAchievement', 'TGW2AccountBankItem', 'TGW2AccountFinisher',
    'TGW2AccountInventoryItem', 'TGW2AccountMastery', 'TGW2AccountMaterial',
    'TGW2AccountWalletItem', 'TGW2AchievementTier', 'TGW2AchievementReward',
    'TGW2AchievementBit', 'TGW2Achievement', 'TErrorMessage', 'TUrlParam',
    'TStateHoler',
    // Werewolf types
    'TIntegerArray', 'TStringArray', 'TRGBArray',
    'TUrlParams', 'TGW2ColorArray', 'TGW2QuagganArray',
    'TGW2WorldArray', 'TGW2CurrencyArray', 'TGW2FileArray',
    'TGW2MiniArray', 'TGW2AccountAchievementArray', 'TGW2AccountBankItemArray',
    'TGW2AccountFinisherArray', 'TGW2AccountInventoryItemArray',
    'TGW2AccountMasteryArray', 'TGW2AccountMaterialArray',
    'TGW2AccountWalletItemArray', 'TGW2AchievementTierArray',
    'TGW2AchievementRewardArray', 'TGW2AchievementBitArray',
    'TGW2AchievementArray'
  );

  VAR_TYPE_ALIAS: array[0..VAR_TYPE_COUNT-1] of string = (
    // System default types
    'Byte', 'Shortint', 'Smallint', 'Word', 'Integer', 'Cardinal', 'Single',
    'Boolean', 'AnsiString', 'String', 'array of const', 'array of Integer',
    'array of string',
    // Generics types
    'TEnum',
    // GW2DA custom types
    'TAPIVersion', 'TAPIFunction', 'TAPIPermissions', 'TWebHandler',
    'TGW2Token', 'TGW2Version', 'TGW2ColorDetail', 'TGW2Color', 'TGW2Quaggan',
    'TGW2World', 'TGW2Currency', 'TGW2File', 'TGW2Mini', 'TGW2Account',
    'TGW2AccountAchievement', 'TGW2AccountBankItem', 'TGW2AccountFinisher',
    'TGW2AccountInventoryItem', 'TGW2AccountMastery', 'TGW2AccountMaterial',
    'TGW2AccountWalletItem', 'TGW2AchievementTier', 'TGW2AchievementReward',
    'TGW2AchievementBit', 'TGW2Achievement', 'TErrorMessage', 'TUrlParam',
    'TStateHoler',
    // Werewolf types
    'array of Integer', 'array of string', 'array [0..2] of Integer',
    'array of TUrlParam', 'array of TGW2Color', 'array of TGW2Quaggan',
    'array of TGW2World', 'array of TGW2Currency', 'array of TGW2File',
    'array of TGW2Mini', 'array of TGW2AccountAchievement',
    'array of TGW2AccountBankItem', 'array of TGW2AccountFinisher',
    'array of TGW2AccountInventoryItem', 'array of TGW2AccountMastery',
    'array of TGW2AccountMaterial', 'array of TGW2AccountWalletItem',
    'array of TGW2AchievementTier', 'array of TGW2AchievementReward',
    'array of TGW2AchievementBit', 'array of TGW2Achievement'
  );

var
  Form1: TForm1;

implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Click;
end;


procedure TForm1.Reinit;
var
  Settings: TINIFile;
begin
  Settings := TINIFile.Create(fSettingsPath);

  if not FileExists(fSettingsPath) then
  begin
    Settings.WriteString('FILES', 'Input',  '..\..\..\..\src\GW2DelphiAPI.pas');
    Settings.WriteString('FILES', 'Output', '.\APIRef.wiki');
  end;

  edtInputFile.Text  := Settings.ReadString('FILES', 'Input',  '..\..\..\..\src\GW2DelphiAPI.pas');
  edtOutputFile.Text := Settings.ReadString('FILES', 'Output', '.\APIRef.wiki');
  FreeAndNil(Settings);

  fSafeToWrite := True;
end;


{
  Parses the param string into prefered wiki-format.
  Results:
  1 - [name]: [type];
  2 - etc
}
function TForm1.ParseParams(aString: string; aDescriptions: TStringList): string;
var
  i, j, K, nextType: Integer;
  isParam: Boolean;
  listTokens, paramList, typeList: TStringList;
  paramHolder: array of TParamHolder;
  lastType: string;
begin
  Result := '';

  listTokens := TStringList.Create;
  paramList := TStringList.Create;
  typeList  := TStringList.Create;
  try
    // If not set to -1 it skips the first variable
    nextType := -1;

    listTokens.AddStrings(aString.Split([' ']));

    // Re-combine type arrays
    for i := 0 to listTokens.Count - 1 do
    begin
      listTokens[i] := listTokens[i].TrimRight([',', ':', ';']);

      if SameText(listTokens[i], 'array') then
      begin
        nextType := i + 2;
        // For some reason this kept giving 'array of Integer;' hence the trim
        paramList.Add((listTokens[i] + ' ' + listTokens[nextType - 1] + ' ' + listTokens[nextType]).TrimRight([',', ':', ';']));
      end else
        // Skip unused stuff
        if not ((SameText(listTokens[i], 'of')) or (SameText(listTokens[i], 'const')) or (i = nextType)) then
          paramList.Add(listTokens[i]);
    end;

    // Bind variable names to their type
    // Use reverse scan, so that we can remember last met type and apply it to all preceeding parameters
    lastType := '';
    for i := paramList.Count - 1 downto 0 do
    begin
      // See if this token is a Type
      isParam := True;
      for K := 0 to High(VAR_TYPE_NAME) do
        if SameText(VAR_TYPE_NAME[K], paramList[i]) then
        begin
          lastType := VAR_TYPE_ALIAS[K];
          isParam := False;
          Break;
        end;

      if isParam then
      begin
        SetLength(paramHolder, Length(paramHolder) + 1);
        paramHolder[High(paramHolder)].Name := paramList[i];
        paramHolder[High(paramHolder)].varType := lastType;
      end;
    end;

    // Add line-breaks
    for i := High(paramHolder) downto 0 do
    begin
      Result := Result + paramHolder[i].Name + ': ' + paramHolder[i].varType + ';';

      // Add micro descriptions to the parameters and remove them from the stringlist.
      for j := aDescriptions.Count - 1 downto 0 do
        if aDescriptions[j].StartsWith(paramHolder[i].Name) then
        begin
          Result := Result + ' // ' + aDescriptions[j].Substring(aDescriptions[j].IndexOf(':') + 2);
          aDescriptions.Delete(j);
          Break;
        end;

      if i <> 0 then
        Result := Result + ' <br> ';
    end;
  finally
    FreeAndNil(listTokens);
    FreeAndNil(paramList);
    FreeAndNil(typeList);
  end;
end;


// Scans file's contents and puts it all in proper formatting for most wikis.
procedure TForm1.ParseText(aFile: string; aList: TStringList; aHasReturn: Boolean);
var
  i, j, iPlus: Integer;
  restStr: string;
  sourceTxt, descrTxt: TStringList;
  res: TCommandInfo;
begin
  sourceTxt := TStringList.Create;
  descrTxt  := TStringList.Create;
  try
    sourceTxt.LoadFromFile(aFile);

    for i := 0 to SourceTxt.Count - 1 do
    begin
      // Reset old values
      res.Version     := '';
      res.Group       := '';
      res.Name        := '';
      res.Description := '';
      res.Parameters  := '';
      res.Return      := '';
      res.ReturnDesc  := '';
      iPlus := 0;
      descrTxt.Clear;

      //* Version: 1234
      //* Class: SomeClass
      //* Large description of the method, optional
      //* aX: Small optional description of parameter
      //* aY: Small optional description of parameter
      //* Result: Small optional description of returned value

      // Before anything it should start with "//* Version:"
      if sourceTxt[i].StartsWith('//* Version:') then
      begin
        restStr     := Trim(sourceTxt[i].Substring(sourceTxt[i].IndexOf(':') + 2));
        res.Version := IfThen(restStr = '', '-', restStr);
        Inc(iPlus);

        if sourceTxt[i+iPlus].StartsWith('//* Class:') then
        begin
          restStr   := Trim(sourceTxt[i+iPlus].Substring(sourceTxt[i+iPlus].IndexOf(':') + 2));
          res.Group := IfThen(restStr = '', '-', restStr);
          Inc(iPlus);
        end;


        // Descriptions are only added by lines starting with "//* "
        if sourceTxt[i+iPlus].StartsWith('//* ') then
          // Repeat until no description tags are found
          while sourceTxt[i+iPlus].StartsWith('//* ') do
          begin
            // Handle Result description separately to keep the output clean.
            if sourceTxt[i+iPlus].StartsWith('//* Result:') then
              res.ReturnDesc := sourceTxt[i+iPlus].Substring(sourceTxt[i+iPlus].IndexOf(':') + 2)
            else
              descrTxt.Add(sourceTxt[i+iPlus].Substring(sourceTxt[i+iPlus].IndexOf('*') + 2));
            Inc(iPlus);
          end;

        // Skip empty or "faulty" lines
        while not (sourceTxt[i+iPlus].StartsWith('constructor') or
                   sourceTxt[i+iPlus].StartsWith('procedure')   or
                   sourceTxt[i+iPlus].StartsWith('function'))   do
          Inc(iPlus);

        // Format procedures/constructors
        if (sourceTxt[i+iPlus].StartsWith('procedure') or sourceTxt[i+iPlus].StartsWith('constructor')) then
        begin
          if sourceTxt[i+iPlus].Contains('(') then
          begin
            res.Name := Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('.') + 2,
                             sourceTxt[i+iPlus].IndexOf('(') - (sourceTxt[i+iPlus].IndexOf('.') + 1));
            res.Parameters := ParseParams(Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('(') + 2,
                                                                   sourceTxt[i+iPlus].IndexOf(')') - (
                                                                   sourceTxt[i+iPlus].IndexOf('(') + 1)), descrTxt);
          end else
          begin
            res.Name := Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('.') + 2,
                             sourceTxt[i+iPlus].IndexOf(';') - (sourceTxt[i+iPlus].IndexOf('.') + 1));
          end;
        end;

        // Format functions
        if sourceTxt[i+iPlus].StartsWith('function') then
        begin
          if sourceTxt[i+iPlus].Contains('(') then
          begin
            res.Name := Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('.') + 2,
                             sourceTxt[i+iPlus].IndexOf('(') - (sourceTxt[i+iPlus].IndexOf('.') + 1));
            res.Parameters := ParseParams(Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('(') + 2,
                                                                   sourceTxt[i+iPlus].IndexOf(')') - (
                                                                   sourceTxt[i+iPlus].IndexOf('(') + 1)), descrTxt);
          end else
          begin
            res.Name := Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('.') + 2,
                             sourceTxt[i+iPlus].IndexOf(':') - (sourceTxt[i+iPlus].IndexOf('.') + 1));
          end;

          restStr  := sourceTxt[i+iPlus].Substring(sourceTxt[i+iPlus].LastIndexOf(':') + 2).TrimRight([';']);;
          res.Return  := IfThen(SameText(restStr, 'TIntegerArray'), 'array of Integer', restStr);
          res.Return  := IfThen(SameText(restStr, 'TStringArray'), 'array of string', restStr);
        end;

        // Now we can assemble Description, after we have detected and removed parameters descriptions from it
        for j := 0 to descrTxt.Count - 1 do
          res.Description := res.Description + ' ' + descrTxt[j];

        // Now we have all the parts and can combine them however we like
        aList.Add('| ' + res.Version + ' | ' + res.Group + '.' + res.Name + '<br><sub>' + res.Description + '</sub>' +
                  ' | <sub>' + res.Parameters + '</sub>' +
                  IfThen(aHasReturn, ' | <sub>' + res.Return + IfThen(res.ReturnDesc <> '', ' // ' + res.ReturnDesc) + '</sub>') +
                  ' |');
      end;
    end;
  finally
    FreeAndNil(sourceTxt);
    FreeAndNil(descrTxt);
  end;
end;

function DoSort(List: TStringList; Index1, Index2: Integer): Integer;
var
  A, B: string;
begin
  A := List[Index1];
  B := List[Index2];
  // Sort in assumption that method name is in the second || clause
  A := Copy(A, PosEx('| ', A, 2) + 2, 40);
  B := Copy(B, PosEx('| ', B, 2) + 2, 40);
  Result := CompareText(A, B);
end;

procedure TForm1.btnGenerateClick(Sender: TObject);
var
  StrList: TStringList;
begin
  txtParserOutput.Lines.Clear;

  if FileExists(edtInputFile.Text) then
  begin
    StrList := TStringList.Create;

    ParseText(edtInputFile.Text, StrList, True);
    StrList.CustomSort(DoSort);

    StrList.Insert(0, '####API reference' + sLineBreak);
    StrList.Insert(1, '| Ver<br>sion | Description | Parameters<br>and types | Returns |');
    StrList.Insert(2, '| ------- | --------------- | -------------------- | ------- |');

    txtParserOutput.Lines.AddStrings(StrList);

    if edtOutputFile.Text <> '' then
      StrList.SaveToFile(edtOutputFile.Text);

    FreeAndNil(StrList);
  end else
    raise Exception.Create('Input file does not exist!');
end;


procedure TForm1.txtParserOutputKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^A then
  begin
    (Sender as TMemo).SelectAll;
    Key := #0;
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  fSettingsPath := ExtractFilePath(Application.ExeName) + 'ScriptingParser.ini';
  Reinit;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  fSettingsPath := ExtractFilePath(Application.ExeName) + 'ScriptingParser2.ini';
  Reinit;
end;


procedure TForm1.edtOnTextChange(Sender: TObject);
var
  Settings: TINIFile;
begin
  if not fSafeToWrite then Exit;

  Settings := TINIFile.Create(fSettingsPath);

  if Sender = edtInputFile then
    Settings.WriteString('FILES', 'Input', edtInputFile.Text);

  if Sender = edtOutputFile then
    Settings.WriteString('FILES', 'Output', edtOutputFile.Text);

  FreeAndNil(Settings);
end;


end.
