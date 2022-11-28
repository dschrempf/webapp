{ config, lib, ... }:

with lib;
let cfg = config.services.webapp;
in
{
  options.services.webapp = {
    enable = mkEnableOption "Enables the Webapp service";
  };

  config = mkIf cfg.enable {
    systemd.services."webapp" = {
      wantedBy = [ "multi-user.target" ];

      serviceConfig =
        let pkg = self.packages.${system}.webapp;
        in
        {
          Restart = "on-failure";
          ExecStart = "${pkg}/bin/webapp";
        };
    };
  };
}
