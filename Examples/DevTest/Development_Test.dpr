program Development_Test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas';

CONST
  { These keys only exist for development purposes and will be deleted ASAP! }
  CONST_DEV_API_KEY = //'FDB8B876-3F72-CF42-866E-987B3384CCDAC5B93645-3BBA-4D65-9D7A-2FA518712370'; // Only Account, Character and Guild
                      //'9AA549AC-E2C5-994C-86D0-F49D255886DFB57AE877-A650-474A-8EC0-33FB67B3D12B'; // Less priv
                      'C94A01D8-5B6E-A044-B6BA-0040B4E177BEC3F9C17B-FD49-43BB-87EC-282DB2C11493'; // All priv

var
  fGW2API:         TGW2API;
  fAPIRequestUrl:  string;
  //fStringArr:      TStringArray;
  //fIntegerArr:     TIntegerArray;
  fTmpString:      string;
  fString:         string;
  fInteger:        Integer;
  //fParams:         TUrlParams;
  fAccount:        TGW2Account;
  fBankItemArr:    TGW2AccountBankItemArray;
  fBankItem:       TGW2AccountBankItem;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    fGW2API := TGW2API.Create();

    WriteLn;
    WriteLn;
    WriteLn('GuildWars build: ' +
            IntToStr(fGW2API.Misc.GetBuild(fGW2API.WebHandler).id));

    WriteLn;
    WriteLn;
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv1];
    WriteLn('Using API version: ' +
            fGW2API.Utils.EnumToString(
              fGW2API.Utils.StringToEnum<TAPIVersion>('APIv1')
            ) + sLineBreak +
            fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl));
    WriteLn;
    WriteLn;
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv2];
    WriteLn('Using API version: ' +
            IntToStr(fGW2API.Utils.EnumToInt(APIv2)) + sLineBreak +
            fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl));

    WriteLn;
    WriteLn;
    WriteLn('Authentication:');
    fTmpString := fGW2API.Authenticate(CONST_DEV_API_KEY);
    WriteLn(fTmpString);
    fTmpString := '';
    WriteLn;
    WriteLn('ID: '   + fGW2API.State.AuthToken.Id);
    WriteLn('Name: ' + fGW2API.State.AuthToken.Name);

    for fString in fGW2API.State.AuthToken.Permissions do
      if fTmpString = '' then
        fTmpString := fString
      else
        fTmpString := fTmpString + ', ' + fString;

    WriteLn('Permissions: ' + fTmpString);

    WriteLn;
    WriteLn;
    WriteLn('Account:');
    fAccount   := fGW2API.Account.GetAccount(fGW2API.WebHandler, fGW2API.State);
    fTmpString := '';
    WriteLn;
    WriteLn('ID: '    +     fAccount.id);
    WriteLn('Name: '  +     fAccount.Name);
    WriteLn('World: ' +     IntToStr(fAccount.World));
    WriteLn('Commander: ' + BoolToStr(fAccount.Commander));

    for fString in fAccount.Guilds do
      if fTmpString = '' then
        fTmpString := fString
      else
        fTmpString := fTmpString + ', ' + fString;

    WriteLn('Guilds: '        + fTmpString);
    WriteLn('Created: '       + fAccount.Created);
    WriteLn('Access: '        + fAccount.Access);
    WriteLn('Fractal_level: ' + IntToStr(fAccount.Fractal_level));
    WriteLn('Daily_ap: '      + IntToStr(fAccount.Daily_ap));
    WriteLn('Monthly_ap: '    + IntToStr(fAccount.Monthly_ap));
    WriteLn('Wvw_rank: '      + IntToStr(fAccount.Wvw_rank));

    WriteLn;
    WriteLn;
    fBankItemArr := fGW2API.Account.GetBank(fGW2API.WebHandler, fGW2API.State);
    WriteLn('Account Bank:');

    for fBankItem in fBankItemArr do
    begin
      if fBankItem = nil then
      begin
        WriteLn(#9 + 'Empty');
        WriteLn;
        Continue;
      end;

      WriteLn(#9 + 'ID: '        + IntToStr(fBankItem.id));
      WriteLn(#9 + 'Count: '     + IntToStr(fBankItem.Count));
      WriteLn(#9 + 'Skin: '      + IntToStr(fBankItem.Skin));
      fTmpString := '';

      for fInteger in fBankItem.Upgrades do
        if fTmpString = '' then
          fTmpString := IntToStr(fInteger)
        else
          fTmpString := fTmpString + ', ' + IntToStr(fInteger);

      WriteLn(#9 + 'Upgrades: '  + fTmpString);
      fTmpString := '';

      for fInteger in fBankItem.Infusions do
        if fTmpString = '' then
          fTmpString := IntToStr(fInteger)
        else
          fTmpString := fTmpString + ', ' + IntToStr(fInteger);

      WriteLn(#9 + 'Infusions: ' + fTmpString);
      WriteLn(#9 + 'Binding: '   + fBankItem.Binding);
      WriteLn(#9 + 'Bound_to: '  + fBankItem.Bound_to);
      WriteLn;
    end;

    FreeAndNil(fGW2API);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  WriteLn('Press the Enter key to continue...');
  ReadLn;
end.
