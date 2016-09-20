unit GW2DA_Authentication;

interface
uses
  SysUtils;

type
  TPermission = Cardinal;

  TPermissions = (
    None,
    PermAccount,
    PermCharacter,
    PermInventory,
    PermTradingpost,
    PermWallet,
    PermUnlocks,
    PermPvP,
    PermBuilds,
    PermProgression,
    PermGuilds,
    PermSize
  );

  TToken = record
    ID:          string;
    Name:        string;
	  Permissions: array of string;
  end;

implementation

end.
