# My Dotfiles
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

## Systems

| System | Architecture | Roles |
|--------|--------------|-------|
| Ivys-MacBook-Pro | aarch64-darwin | study, gui, dev |
| auspc | x86_64-linux | gui, gaming, dev |
| contabo | x86_64-linux |  |
| imflopet | x86_64-linux |  |
| lora-pi | aarch64-linux |  |
| macmini | aarch64-darwin | gui |
| pentestvm | x86_64-linux |  |
| secondpc | x86_64-linux |  |
| surfacelaptop | x86_64-linux | gui, dev |
| wsl-nixos | x86_64-linux |  |

## Ivys-MacBook-Pro

**Architecture:** `aarch64-darwin`
**Roles:** study, gui, dev

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  Ivys_MacBook_Pro([Ivys-MacBook-Pro]):::root

  subgraph ctx_host_Ivys_MacBook_Pro["host: Ivys-MacBook-Pro"]
  _anon_["<anon>"]:::_anon__c
  _policy__policy_onepassword_role_gui__132___to_hosts__0_["<policy:<policy:onepassword-role-gui>[132]/to-hosts>[0]"]:::_policy__policy_onepassword_role_gui__132___to_hosts__0__c
  _policy__policy_onepassword_role_gui__132___to_users__1_["<policy:<policy:onepassword-role-gui>[132]/to-users>[1]"]:::_policy__policy_onepassword_role_gui__132___to_users__1__c
  _policy_Ivys_MacBook_Pro_role_dev__2_["<policy:Ivys-MacBook-Pro-role-dev>[2]"]:::_policy_Ivys_MacBook_Pro_role_dev__2__c
  _policy_Ivys_MacBook_Pro_role_gui__3_["<policy:Ivys-MacBook-Pro-role-gui>[3]"]:::_policy_Ivys_MacBook_Pro_role_gui__3__c
  _policy_Ivys_MacBook_Pro_role_study__4_["<policy:Ivys-MacBook-Pro-role-study>[4]"]:::_policy_Ivys_MacBook_Pro_role_study__4__c
  _policy_ccache_role_dev__6_["<policy:ccache-role-dev>[6]"]:::_policy_ccache_role_dev__6__c
  _policy_ccache_role_gui__7_["<policy:ccache-role-gui>[7]"]:::_policy_ccache_role_gui__7__c
  _policy_ccache_role_study__8_["<policy:ccache-role-study>[8]"]:::_policy_ccache_role_study__8__c
  _policy_darwin_base_role_dev__10_["<policy:darwin-base-role-dev>[10]"]:::_policy_darwin_base_role_dev__10__c
  _policy_darwin_base_role_gui__11_["<policy:darwin-base-role-gui>[11]"]:::_policy_darwin_base_role_gui__11__c
  _policy_darwin_base_role_study__12_["<policy:darwin-base-role-study>[12]"]:::_policy_darwin_base_role_study__12__c
  _policy_darwin_finder_role_dev__13_["<policy:darwin-finder-role-dev>[13]"]:::_policy_darwin_finder_role_dev__13__c
  _policy_darwin_finder_role_gui__14_["<policy:darwin-finder-role-gui>[14]"]:::_policy_darwin_finder_role_gui__14__c
  _policy_darwin_finder_role_study__15_["<policy:darwin-finder-role-study>[15]"]:::_policy_darwin_finder_role_study__15__c
  _policy_darwin_general_role_dev__16_["<policy:darwin-general-role-dev>[16]"]:::_policy_darwin_general_role_dev__16__c
  _policy_darwin_general_role_gui__17_["<policy:darwin-general-role-gui>[17]"]:::_policy_darwin_general_role_gui__17__c
  _policy_darwin_general_role_study__18_["<policy:darwin-general-role-study>[18]"]:::_policy_darwin_general_role_study__18__c
  _policy_darwin_hmApps_role_dev__19_["<policy:darwin-hmApps-role-dev>[19]"]:::_policy_darwin_hmApps_role_dev__19__c
  _policy_darwin_hmApps_role_gui__20_["<policy:darwin-hmApps-role-gui>[20]"]:::_policy_darwin_hmApps_role_gui__20__c
  _policy_darwin_hmApps_role_study__21_["<policy:darwin-hmApps-role-study>[21]"]:::_policy_darwin_hmApps_role_study__21__c
  _policy_karabiner_driver_role_dev__22_["<policy:karabiner-driver-role-dev>[22]"]:::_policy_karabiner_driver_role_dev__22__c
  _policy_karabiner_driver_role_gui__23_["<policy:karabiner-driver-role-gui>[23]"]:::_policy_karabiner_driver_role_gui__23__c
  _policy_karabiner_driver_role_study__24_["<policy:karabiner-driver-role-study>[24]"]:::_policy_karabiner_driver_role_study__24__c
  _policy_laptop_brew_role_dev__25_["<policy:laptop-brew-role-dev>[25]"]:::_policy_laptop_brew_role_dev__25__c
  _policy_laptop_brew_role_gui__26_["<policy:laptop-brew-role-gui>[26]"]:::_policy_laptop_brew_role_gui__26__c
  _policy_laptop_brew_role_study__27_["<policy:laptop-brew-role-study>[27]"]:::_policy_laptop_brew_role_study__27__c
  _policy_lix_role_dev__28_["<policy:lix-role-dev>[28]"]:::_policy_lix_role_dev__28__c
  _policy_lix_role_gui__29_["<policy:lix-role-gui>[29]"]:::_policy_lix_role_gui__29__c
  _policy_lix_role_study__30_["<policy:lix-role-study>[30]"]:::_policy_lix_role_study__30__c
  _policy_nixos_general_role_dev__31_["<policy:nixos-general-role-dev>[31]"]:::_policy_nixos_general_role_dev__31__c
  _policy_nixos_general_role_gui__32_["<policy:nixos-general-role-gui>[32]"]:::_policy_nixos_general_role_gui__32__c
  _policy_nixos_general_role_study__33_["<policy:nixos-general-role-study>[33]"]:::_policy_nixos_general_role_study__33__c
  _policy_openssh_role_dev__34_["<policy:openssh-role-dev>[34]"]:::_policy_openssh_role_dev__34__c
  _policy_openssh_role_gui__35_["<policy:openssh-role-gui>[35]"]:::_policy_openssh_role_gui__35__c
  _policy_openssh_role_study__36_["<policy:openssh-role-study>[36]"]:::_policy_openssh_role_study__36__c
  _policy_pam_touchid_role_dev__37_["<policy:pam-touchid-role-dev>[37]"]:::_policy_pam_touchid_role_dev__37__c
  _policy_pam_touchid_role_gui__38_["<policy:pam-touchid-role-gui>[38]"]:::_policy_pam_touchid_role_gui__38__c
  _policy_pam_touchid_role_study__39_["<policy:pam-touchid-role-study>[39]"]:::_policy_pam_touchid_role_study__39__c
  _policy_sccache_role_dev__40_["<policy:sccache-role-dev>[40]"]:::_policy_sccache_role_dev__40__c
  _policy_sccache_role_gui__41_["<policy:sccache-role-gui>[41]"]:::_policy_sccache_role_gui__41__c
  _policy_sccache_role_study__42_["<policy:sccache-role-study>[42]"]:::_policy_sccache_role_study__42__c
  _policy_sudoagents_role_dev__43_["<policy:sudoagents-role-dev>[43]"]:::_policy_sudoagents_role_dev__43__c
  _policy_sudoagents_role_gui__44_["<policy:sudoagents-role-gui>[44]"]:::_policy_sudoagents_role_gui__44__c
  _policy_sudoagents_role_study__45_["<policy:sudoagents-role-study>[45]"]:::_policy_sudoagents_role_study__45__c
  _policy_to_users_role_dev__46_["<policy:to-users-role-dev>[46]"]:::_policy_to_users_role_dev__46__c
  _policy_to_users_role_gui__47_["<policy:to-users-role-gui>[47]"]:::_policy_to_users_role_gui__47__c
  _policy_to_users_role_study__48_["<policy:to-users-role-study>[48]"]:::_policy_to_users_role_study__48__c
  _policy_vpn_role_dev__49_["<policy:vpn-role-dev>[49]"]:::_policy_vpn_role_dev__49__c
  _policy_vpn_role_gui__50_["<policy:vpn-role-gui>[50]"]:::_policy_vpn_role_gui__50__c
  _policy_vpn_role_study__51_["<policy:vpn-role-study>[51]"]:::_policy_vpn_role_study__51__c
  _policy_vpn_secrets_role_dev__52_["<policy:vpn-secrets-role-dev>[52]"]:::_policy_vpn_secrets_role_dev__52__c
  _policy_vpn_secrets_role_gui__53_["<policy:vpn-secrets-role-gui>[53]"]:::_policy_vpn_secrets_role_gui__53__c
  _policy_vpn_secrets_role_study__54_["<policy:vpn-secrets-role-study>[54]"]:::_policy_vpn_secrets_role_study__54__c
  _policy_vpn_ssh_config_role_dev__55_["<policy:vpn-ssh-config-role-dev>[55]"]:::_policy_vpn_ssh_config_role_dev__55__c
  _policy_vpn_ssh_config_role_gui__56_["<policy:vpn-ssh-config-role-gui>[56]"]:::_policy_vpn_ssh_config_role_gui__56__c
  _policy_vpn_ssh_config_role_study__57_["<policy:vpn-ssh-config-role-study>[57]"]:::_policy_vpn_ssh_config_role_study__57__c
  _policy_vpn_ssh_config__to_users__58_["<policy:vpn-ssh-config/to-users>[58]"]:::_policy_vpn_ssh_config__to_users__58__c
  _policy_zen_classes_role_dev__59_["<policy:zen-classes-role-dev>[59]"]:::_policy_zen_classes_role_dev__59__c
  _policy_zen_classes_role_gui__60_["<policy:zen-classes-role-gui>[60]"]:::_policy_zen_classes_role_gui__60__c
  _policy_zen_classes_role_study__61_["<policy:zen-classes-role-study>[61]"]:::_policy_zen_classes_role_study__61__c
  _policy_zotero_role_dev__62_["<policy:zotero-role-dev>[62]"]:::_policy_zotero_role_dev__62__c
  _policy_zotero_role_gui__63_["<policy:zotero-role-gui>[63]"]:::_policy_zotero_role_gui__63__c
  _policy_zotero_role_study__64_["<policy:zotero-role-study>[64]"]:::_policy_zotero_role_study__64__c
  ccache__Ivys_MacBook_Pro[/"ccache/Ivys-MacBook-Pro"\]:::ccache__Ivys_MacBook_Pro_c
  agenix_rekey_host_Ivys_MacBook_Pro["agenix-rekey"]:::agenix_rekey_host_Ivys_MacBook_Pro_c
  ccache["ccache"]:::ccache_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_Ivys_MacBook_Pro["default"]:::default_host_Ivys_MacBook_Pro_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro{{"batteries/define-user/ivypierlot@Ivys-MacBook-Pro"}}:::den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro_c
  difftastic["difftastic"]:::difftastic_c
  extra_registry["extra-registry"]:::extra_registry_c
  fonts_host_Ivys_MacBook_Pro["fonts"]:::fonts_host_Ivys_MacBook_Pro_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  homebrew_host_Ivys_MacBook_Pro["homebrew"]:::homebrew_host_Ivys_MacBook_Pro_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__4_["host/resolve(<when>:4)"]:::host__resolve__when__4__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  host__resolve_vpn_{{"host/resolve(vpn)"}}:::host__resolve_vpn__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  karabiner_driver["karabiner-driver"]:::karabiner_driver_c
  kind_system_routes_host_Ivys_MacBook_Pro["kind-system-routes"]:::kind_system_routes_host_Ivys_MacBook_Pro_c
  lib["lib"]:::lib_c
  lix["lix"]:::lix_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_Ivys_MacBook_Pro["nix-to-host"]:::nix_to_host_host_Ivys_MacBook_Pro_c
  nix___when__5["nix/<when>:5"]:::nix___when__5_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_Ivys_MacBook_Pro["nixpkgs-config"]:::nixpkgs_config_host_Ivys_MacBook_Pro_c
  nixvim_include_global_pkgs_host_Ivys_MacBook_Pro["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_Ivys_MacBook_Pro_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_Ivys_MacBook_Pro["os-to-host"]:::os_to_host_host_Ivys_MacBook_Pro_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_Ivys_MacBook_Pro["overlays-to-_overlays"]:::overlays_to__overlays_host_Ivys_MacBook_Pro_c
  overlays_to_flake_parts_host_Ivys_MacBook_Pro["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_Ivys_MacBook_Pro_c
  pam_rssh_host_Ivys_MacBook_Pro["pam-rssh"]:::pam_rssh_host_Ivys_MacBook_Pro_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_Ivys_MacBook_Pro["pipe-unfree"]:::pipe_unfree_host_Ivys_MacBook_Pro_c
  route_casks_host_Ivys_MacBook_Pro["route-casks"]:::route_casks_host_Ivys_MacBook_Pro_c
  sccache["sccache"]:::sccache_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_Ivys_MacBook_Pro["shell"]:::shell_host_Ivys_MacBook_Pro_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  sudoagents_host_Ivys_MacBook_Pro["sudoagents"]:::sudoagents_host_Ivys_MacBook_Pro_c
  ivypierlot__Ivys_MacBook_Pro__to_users[/"Ivys-MacBook-Pro/to-users"\]:::ivypierlot__Ivys_MacBook_Pro__to_users_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  vpn_ssh_config["vpn-ssh-config"]:::vpn_ssh_config_c
  Ivys_MacBook_Pro --> ccache
  Ivys_MacBook_Pro --> homebrew_host_Ivys_MacBook_Pro
  Ivys_MacBook_Pro --> karabiner_driver
  Ivys_MacBook_Pro --> lix
  Ivys_MacBook_Pro --> sccache
  Ivys_MacBook_Pro --> sudoagents_host_Ivys_MacBook_Pro
  Ivys_MacBook_Pro --> vpn
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_Ivys_MacBook_Pro --> den__batteries__define_user
  default_host_Ivys_MacBook_Pro --> extra_registry
  default_host_Ivys_MacBook_Pro --> home_base
  default_host_Ivys_MacBook_Pro -.-x host__resolve_default_
  default_host_Ivys_MacBook_Pro --> den__batteries__hostname
  default_host_Ivys_MacBook_Pro --> den__batteries__inputs_
  default_host_Ivys_MacBook_Pro --> insecure_predicate
  default_host_Ivys_MacBook_Pro --> lib
  default_host_Ivys_MacBook_Pro --> nix
  default_host_Ivys_MacBook_Pro --> overlays
  default_host_Ivys_MacBook_Pro --> den__batteries__self_
  default_host_Ivys_MacBook_Pro --> den__batteries__sources
  default_host_Ivys_MacBook_Pro --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_Ivys_MacBook_Pro --> fonts___when__4
  fonts_host_Ivys_MacBook_Pro --> fonts___when__5
  home_base --> shell_host_Ivys_MacBook_Pro
  host --> Ivys_MacBook_Pro
  host --> darwin_base
  host --> default_host_Ivys_MacBook_Pro
  host --> fonts_host_Ivys_MacBook_Pro
  host --> host__resolve_host_
  host --> nixos_general
  host --> nixpkgs_config_host_Ivys_MacBook_Pro
  host --> openssh
  host --> pam_rssh_host_Ivys_MacBook_Pro
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  nix --> nix___when__5
  nixpkgs_config_host_Ivys_MacBook_Pro --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_Ivys_MacBook_Pro --> jujutsu
  shell_host_Ivys_MacBook_Pro --> nix_index
  shell_host_Ivys_MacBook_Pro --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> host__resolve_vpn_
  vpn --> vpn_secrets
  vpn --> vpn_ssh_config
  vpn_secrets --> agenix_rekey_host_Ivys_MacBook_Pro
  ccache -.->|provides| ccache__Ivys_MacBook_Pro
  end
  subgraph ctx_user_ivypierlot["user: ivypierlot"]
  _policy_agenix_rekey_role_dev__0_["<policy:agenix-rekey-role-dev>[0]"]:::_policy_agenix_rekey_role_dev__0__c
  _policy_agenix_rekey_role_gui__1_["<policy:agenix-rekey-role-gui>[1]"]:::_policy_agenix_rekey_role_gui__1__c
  _policy_agenix_rekey_role_study__2_["<policy:agenix-rekey-role-study>[2]"]:::_policy_agenix_rekey_role_study__2__c
  _policy_agenix_rekey__to_users__3_["<policy:agenix-rekey/to-users>[3]"]:::_policy_agenix_rekey__to_users__3__c
  _policy_celler_push_role_dev__5_["<policy:celler-push-role-dev>[5]"]:::_policy_celler_push_role_dev__5__c
  _policy_celler_push_role_gui__6_["<policy:celler-push-role-gui>[6]"]:::_policy_celler_push_role_gui__6__c
  _policy_celler_push_role_study__7_["<policy:celler-push-role-study>[7]"]:::_policy_celler_push_role_study__7__c
  _policy_cotabby_role_dev__8_["<policy:cotabby-role-dev>[8]"]:::_policy_cotabby_role_dev__8__c
  _policy_cotabby_role_gui__9_["<policy:cotabby-role-gui>[9]"]:::_policy_cotabby_role_gui__9__c
  _policy_cotabby_role_study__10_["<policy:cotabby-role-study>[10]"]:::_policy_cotabby_role_study__10__c
  _policy_default_role_dev__11_["<policy:default-role-dev>[11]"]:::_policy_default_role_dev__11__c
  _policy_default_role_gui__12_["<policy:default-role-gui>[12]"]:::_policy_default_role_gui__12__c
  _policy_default_role_study__13_["<policy:default-role-study>[13]"]:::_policy_default_role_study__13__c
  _policy_default__to_hosts__14_["<policy:default/to-hosts>[14]"]:::_policy_default__to_hosts__14__c
  _policy_define_user_role_dev__15_["<policy:define-user-role-dev>[15]"]:::_policy_define_user_role_dev__15__c
  _policy_define_user_role_gui__16_["<policy:define-user-role-gui>[16]"]:::_policy_define_user_role_gui__16__c
  _policy_define_user_role_study__17_["<policy:define-user-role-study>[17]"]:::_policy_define_user_role_study__17__c
  _policy_dev_cli_role_dev__18_["<policy:dev-cli-role-dev>[18]"]:::_policy_dev_cli_role_dev__18__c
  _policy_dev_cli_role_gui__19_["<policy:dev-cli-role-gui>[19]"]:::_policy_dev_cli_role_gui__19__c
  _policy_dev_cli_role_study__20_["<policy:dev-cli-role-study>[20]"]:::_policy_dev_cli_role_study__20__c
  _policy_dev_nix_role_dev__21_["<policy:dev-nix-role-dev>[21]"]:::_policy_dev_nix_role_dev__21__c
  _policy_dev_nix_role_gui__22_["<policy:dev-nix-role-gui>[22]"]:::_policy_dev_nix_role_gui__22__c
  _policy_dev_nix_role_study__23_["<policy:dev-nix-role-study>[23]"]:::_policy_dev_nix_role_study__23__c
  _policy_dev_role_dev__24_["<policy:dev-role-dev>[24]"]:::_policy_dev_role_dev__24__c
  _policy_dev_role_gui__25_["<policy:dev-role-gui>[25]"]:::_policy_dev_role_gui__25__c
  _policy_dev_role_study__26_["<policy:dev-role-study>[26]"]:::_policy_dev_role_study__26__c
  _policy_difftastic_role_dev__27_["<policy:difftastic-role-dev>[27]"]:::_policy_difftastic_role_dev__27__c
  _policy_difftastic_role_gui__28_["<policy:difftastic-role-gui>[28]"]:::_policy_difftastic_role_gui__28__c
  _policy_difftastic_role_study__29_["<policy:difftastic-role-study>[29]"]:::_policy_difftastic_role_study__29__c
  _policy_eagle_nvim_role_dev__30_["<policy:eagle-nvim-role-dev>[30]"]:::_policy_eagle_nvim_role_dev__30__c
  _policy_eagle_nvim_role_gui__31_["<policy:eagle-nvim-role-gui>[31]"]:::_policy_eagle_nvim_role_gui__31__c
  _policy_eagle_nvim_role_study__32_["<policy:eagle-nvim-role-study>[32]"]:::_policy_eagle_nvim_role_study__32__c
  _policy_extra_registry_role_dev__33_["<policy:extra-registry-role-dev>[33]"]:::_policy_extra_registry_role_dev__33__c
  _policy_extra_registry_role_gui__34_["<policy:extra-registry-role-gui>[34]"]:::_policy_extra_registry_role_gui__34__c
  _policy_extra_registry_role_study__35_["<policy:extra-registry-role-study>[35]"]:::_policy_extra_registry_role_study__35__c
  _policy_file_local_role_dev__36_["<policy:file-local-role-dev>[36]"]:::_policy_file_local_role_dev__36__c
  _policy_file_local_role_gui__37_["<policy:file-local-role-gui>[37]"]:::_policy_file_local_role_gui__37__c
  _policy_file_local_role_study__38_["<policy:file-local-role-study>[38]"]:::_policy_file_local_role_study__38__c
  _policy_fish_role_dev__39_["<policy:fish-role-dev>[39]"]:::_policy_fish_role_dev__39__c
  _policy_fish_role_gui__40_["<policy:fish-role-gui>[40]"]:::_policy_fish_role_gui__40__c
  _policy_fish_role_study__41_["<policy:fish-role-study>[41]"]:::_policy_fish_role_study__41__c
  _policy_fonts_role_dev__43_["<policy:fonts-role-dev>[43]"]:::_policy_fonts_role_dev__43__c
  _policy_fonts_role_gui__44_["<policy:fonts-role-gui>[44]"]:::_policy_fonts_role_gui__44__c
  _policy_fonts_role_study__45_["<policy:fonts-role-study>[45]"]:::_policy_fonts_role_study__45__c
  _policy_ghostty_role_dev__46_["<policy:ghostty-role-dev>[46]"]:::_policy_ghostty_role_dev__46__c
  _policy_ghostty_role_gui__47_["<policy:ghostty-role-gui>[47]"]:::_policy_ghostty_role_gui__47__c
  _policy_ghostty_role_study__48_["<policy:ghostty-role-study>[48]"]:::_policy_ghostty_role_study__48__c
  _policy_gpg_role_dev__49_["<policy:gpg-role-dev>[49]"]:::_policy_gpg_role_dev__49__c
  _policy_gpg_role_gui__50_["<policy:gpg-role-gui>[50]"]:::_policy_gpg_role_gui__50__c
  _policy_gpg_role_study__51_["<policy:gpg-role-study>[51]"]:::_policy_gpg_role_study__51__c
  _policy_gui_role_dev__52_["<policy:gui-role-dev>[52]"]:::_policy_gui_role_dev__52__c
  _policy_gui_role_gui__53_["<policy:gui-role-gui>[53]"]:::_policy_gui_role_gui__53__c
  _policy_gui_role_study__54_["<policy:gui-role-study>[54]"]:::_policy_gui_role_study__54__c
  _policy_hm_user_detect__55_["<policy:hm-user-detect>[55]"]:::_policy_hm_user_detect__55__c
  _policy_home_base_role_dev__57_["<policy:home-base-role-dev>[57]"]:::_policy_home_base_role_dev__57__c
  _policy_home_base_role_gui__58_["<policy:home-base-role-gui>[58]"]:::_policy_home_base_role_gui__58__c
  _policy_home_base_role_study__59_["<policy:home-base-role-study>[59]"]:::_policy_home_base_role_study__59__c
  _policy_homebrew_role_dev__60_["<policy:homebrew-role-dev>[60]"]:::_policy_homebrew_role_dev__60__c
  _policy_homebrew_role_gui__61_["<policy:homebrew-role-gui>[61]"]:::_policy_homebrew_role_gui__61__c
  _policy_homebrew_role_study__62_["<policy:homebrew-role-study>[62]"]:::_policy_homebrew_role_study__62__c
  _policy_hostname_role_dev__63_["<policy:hostname-role-dev>[63]"]:::_policy_hostname_role_dev__63__c
  _policy_hostname_role_gui__64_["<policy:hostname-role-gui>[64]"]:::_policy_hostname_role_gui__64__c
  _policy_hostname_role_study__65_["<policy:hostname-role-study>[65]"]:::_policy_hostname_role_study__65__c
  _policy_idris_role_dev__66_["<policy:idris-role-dev>[66]"]:::_policy_idris_role_dev__66__c
  _policy_idris_role_gui__67_["<policy:idris-role-gui>[67]"]:::_policy_idris_role_gui__67__c
  _policy_idris_role_study__68_["<policy:idris-role-study>[68]"]:::_policy_idris_role_study__68__c
  _policy_inputs__role_dev__69_["<policy:inputs'-role-dev>[69]"]:::_policy_inputs__role_dev__69__c
  _policy_inputs__role_gui__70_["<policy:inputs'-role-gui>[70]"]:::_policy_inputs__role_gui__70__c
  _policy_inputs__role_study__71_["<policy:inputs'-role-study>[71]"]:::_policy_inputs__role_study__71__c
  _policy_insecure_predicate_role_dev__72_["<policy:insecure-predicate-role-dev>[72]"]:::_policy_insecure_predicate_role_dev__72__c
  _policy_insecure_predicate_role_gui__73_["<policy:insecure-predicate-role-gui>[73]"]:::_policy_insecure_predicate_role_gui__73__c
  _policy_insecure_predicate_role_study__74_["<policy:insecure-predicate-role-study>[74]"]:::_policy_insecure_predicate_role_study__74__c
  _policy_ivy_fetch_role_dev__75_["<policy:ivy-fetch-role-dev>[75]"]:::_policy_ivy_fetch_role_dev__75__c
  _policy_ivy_fetch_role_gui__76_["<policy:ivy-fetch-role-gui>[76]"]:::_policy_ivy_fetch_role_gui__76__c
  _policy_ivy_fetch_role_study__77_["<policy:ivy-fetch-role-study>[77]"]:::_policy_ivy_fetch_role_study__77__c
  _policy_ivypierlot_role_dev__78_["<policy:ivypierlot-role-dev>[78]"]:::_policy_ivypierlot_role_dev__78__c
  _policy_ivypierlot_role_gui__79_["<policy:ivypierlot-role-gui>[79]"]:::_policy_ivypierlot_role_gui__79__c
  _policy_ivypierlot_role_study__80_["<policy:ivypierlot-role-study>[80]"]:::_policy_ivypierlot_role_study__80__c
  _policy_jankyborders_role_dev__82_["<policy:jankyborders-role-dev>[82]"]:::_policy_jankyborders_role_dev__82__c
  _policy_jankyborders_role_gui__83_["<policy:jankyborders-role-gui>[83]"]:::_policy_jankyborders_role_gui__83__c
  _policy_jankyborders_role_study__84_["<policy:jankyborders-role-study>[84]"]:::_policy_jankyborders_role_study__84__c
  _policy_jj_mcp_server_role_dev__85_["<policy:jj-mcp-server-role-dev>[85]"]:::_policy_jj_mcp_server_role_dev__85__c
  _policy_jj_mcp_server_role_gui__86_["<policy:jj-mcp-server-role-gui>[86]"]:::_policy_jj_mcp_server_role_gui__86__c
  _policy_jj_mcp_server_role_study__87_["<policy:jj-mcp-server-role-study>[87]"]:::_policy_jj_mcp_server_role_study__87__c
  _policy_jujutsu_role_dev__88_["<policy:jujutsu-role-dev>[88]"]:::_policy_jujutsu_role_dev__88__c
  _policy_jujutsu_role_gui__89_["<policy:jujutsu-role-gui>[89]"]:::_policy_jujutsu_role_gui__89__c
  _policy_jujutsu_role_study__90_["<policy:jujutsu-role-study>[90]"]:::_policy_jujutsu_role_study__90__c
  _policy_kanata_role_dev__91_["<policy:kanata-role-dev>[91]"]:::_policy_kanata_role_dev__91__c
  _policy_kanata_role_gui__92_["<policy:kanata-role-gui>[92]"]:::_policy_kanata_role_gui__92__c
  _policy_kanata_role_study__93_["<policy:kanata-role-study>[93]"]:::_policy_kanata_role_study__93__c
  _policy_lib_role_dev__95_["<policy:lib-role-dev>[95]"]:::_policy_lib_role_dev__95__c
  _policy_lib_role_gui__96_["<policy:lib-role-gui>[96]"]:::_policy_lib_role_gui__96__c
  _policy_lib_role_study__97_["<policy:lib-role-study>[97]"]:::_policy_lib_role_study__97__c
  _policy_llama_cpp_role_dev__98_["<policy:llama-cpp-role-dev>[98]"]:::_policy_llama_cpp_role_dev__98__c
  _policy_llama_cpp_role_gui__99_["<policy:llama-cpp-role-gui>[99]"]:::_policy_llama_cpp_role_gui__99__c
  _policy_llama_cpp_role_study__100_["<policy:llama-cpp-role-study>[100]"]:::_policy_llama_cpp_role_study__100__c
  _policy_lspmux_role_dev__101_["<policy:lspmux-role-dev>[101]"]:::_policy_lspmux_role_dev__101__c
  _policy_lspmux_role_gui__102_["<policy:lspmux-role-gui>[102]"]:::_policy_lspmux_role_gui__102__c
  _policy_lspmux_role_study__103_["<policy:lspmux-role-study>[103]"]:::_policy_lspmux_role_study__103__c
  _policy_main_ssh_key_role_dev__104_["<policy:main-ssh-key-role-dev>[104]"]:::_policy_main_ssh_key_role_dev__104__c
  _policy_main_ssh_key_role_gui__105_["<policy:main-ssh-key-role-gui>[105]"]:::_policy_main_ssh_key_role_gui__105__c
  _policy_main_ssh_key_role_study__106_["<policy:main-ssh-key-role-study>[106]"]:::_policy_main_ssh_key_role_study__106__c
  _policy_main_ssh_key__to_hosts__107_["<policy:main-ssh-key/to-hosts>[107]"]:::_policy_main_ssh_key__to_hosts__107__c
  _policy_mcp_servers_role_dev__108_["<policy:mcp-servers-role-dev>[108]"]:::_policy_mcp_servers_role_dev__108__c
  _policy_mcp_servers_role_gui__109_["<policy:mcp-servers-role-gui>[109]"]:::_policy_mcp_servers_role_gui__109__c
  _policy_mcp_servers_role_study__110_["<policy:mcp-servers-role-study>[110]"]:::_policy_mcp_servers_role_study__110__c
  _policy_neovim_role_dev__111_["<policy:neovim-role-dev>[111]"]:::_policy_neovim_role_dev__111__c
  _policy_neovim_role_gui__112_["<policy:neovim-role-gui>[112]"]:::_policy_neovim_role_gui__112__c
  _policy_neovim_role_study__113_["<policy:neovim-role-study>[113]"]:::_policy_neovim_role_study__113__c
  _policy_nix_index_role_dev__115_["<policy:nix-index-role-dev>[115]"]:::_policy_nix_index_role_dev__115__c
  _policy_nix_index_role_gui__116_["<policy:nix-index-role-gui>[116]"]:::_policy_nix_index_role_gui__116__c
  _policy_nix_index_role_study__117_["<policy:nix-index-role-study>[117]"]:::_policy_nix_index_role_study__117__c
  _policy_nix_role_dev__118_["<policy:nix-role-dev>[118]"]:::_policy_nix_role_dev__118__c
  _policy_nix_role_gui__119_["<policy:nix-role-gui>[119]"]:::_policy_nix_role_gui__119__c
  _policy_nix_role_study__120_["<policy:nix-role-study>[120]"]:::_policy_nix_role_study__120__c
  _policy_nixpkgs_config_role_dev__121_["<policy:nixpkgs-config-role-dev>[121]"]:::_policy_nixpkgs_config_role_dev__121__c
  _policy_nixpkgs_config_role_gui__122_["<policy:nixpkgs-config-role-gui>[122]"]:::_policy_nixpkgs_config_role_gui__122__c
  _policy_nixpkgs_config_role_study__123_["<policy:nixpkgs-config-role-study>[123]"]:::_policy_nixpkgs_config_role_study__123__c
  _policy_nixvim_role_dev__124_["<policy:nixvim-role-dev>[124]"]:::_policy_nixvim_role_dev__124__c
  _policy_nixvim_role_gui__125_["<policy:nixvim-role-gui>[125]"]:::_policy_nixvim_role_gui__125__c
  _policy_nixvim_role_study__126_["<policy:nixvim-role-study>[126]"]:::_policy_nixvim_role_study__126__c
  _policy_nixvim_user_forward__127_["<policy:nixvim-user-forward>[127]"]:::_policy_nixvim_user_forward__127__c
  _policy_nushell_role_dev__128_["<policy:nushell-role-dev>[128]"]:::_policy_nushell_role_dev__128__c
  _policy_nushell_role_gui__129_["<policy:nushell-role-gui>[129]"]:::_policy_nushell_role_gui__129__c
  _policy_nushell_role_study__130_["<policy:nushell-role-study>[130]"]:::_policy_nushell_role_study__130__c
  _policy_onepassword_role_dev__131_["<policy:onepassword-role-dev>[131]"]:::_policy_onepassword_role_dev__131__c
  _policy_onepassword_role_gui__132_["<policy:onepassword-role-gui>[132]"]:::_policy_onepassword_role_gui__132__c
  _policy_onepassword_role_study__133_["<policy:onepassword-role-study>[133]"]:::_policy_onepassword_role_study__133__c
  _policy_openclaw_role_dev__134_["<policy:openclaw-role-dev>[134]"]:::_policy_openclaw_role_dev__134__c
  _policy_openclaw_role_gui__135_["<policy:openclaw-role-gui>[135]"]:::_policy_openclaw_role_gui__135__c
  _policy_openclaw_role_study__136_["<policy:openclaw-role-study>[136]"]:::_policy_openclaw_role_study__136__c
  _policy_opencode_role_dev__137_["<policy:opencode-role-dev>[137]"]:::_policy_opencode_role_dev__137__c
  _policy_opencode_role_gui__138_["<policy:opencode-role-gui>[138]"]:::_policy_opencode_role_gui__138__c
  _policy_opencode_role_study__139_["<policy:opencode-role-study>[139]"]:::_policy_opencode_role_study__139__c
  _policy_overlays_role_dev__140_["<policy:overlays-role-dev>[140]"]:::_policy_overlays_role_dev__140__c
  _policy_overlays_role_gui__141_["<policy:overlays-role-gui>[141]"]:::_policy_overlays_role_gui__141__c
  _policy_overlays_role_study__142_["<policy:overlays-role-study>[142]"]:::_policy_overlays_role_study__142__c
  _policy_pam_rssh_role_dev__143_["<policy:pam-rssh-role-dev>[143]"]:::_policy_pam_rssh_role_dev__143__c
  _policy_pam_rssh_role_gui__144_["<policy:pam-rssh-role-gui>[144]"]:::_policy_pam_rssh_role_gui__144__c
  _policy_pam_rssh_role_study__145_["<policy:pam-rssh-role-study>[145]"]:::_policy_pam_rssh_role_study__145__c
  _policy_rift_role_dev__146_["<policy:rift-role-dev>[146]"]:::_policy_rift_role_dev__146__c
  _policy_rift_role_gui__147_["<policy:rift-role-gui>[147]"]:::_policy_rift_role_gui__147__c
  _policy_rift_role_study__148_["<policy:rift-role-study>[148]"]:::_policy_rift_role_study__148__c
  _policy_rust_role_dev__149_["<policy:rust-role-dev>[149]"]:::_policy_rust_role_dev__149__c
  _policy_rust_role_gui__150_["<policy:rust-role-gui>[150]"]:::_policy_rust_role_gui__150__c
  _policy_rust_role_study__151_["<policy:rust-role-study>[151]"]:::_policy_rust_role_study__151__c
  _policy_self__role_dev__152_["<policy:self'-role-dev>[152]"]:::_policy_self__role_dev__152__c
  _policy_self__role_gui__153_["<policy:self'-role-gui>[153]"]:::_policy_self__role_gui__153__c
  _policy_self__role_study__154_["<policy:self'-role-study>[154]"]:::_policy_self__role_study__154__c
  _policy_shell_role_dev__155_["<policy:shell-role-dev>[155]"]:::_policy_shell_role_dev__155__c
  _policy_shell_role_gui__156_["<policy:shell-role-gui>[156]"]:::_policy_shell_role_gui__156__c
  _policy_shell_role_study__157_["<policy:shell-role-study>[157]"]:::_policy_shell_role_study__157__c
  _policy_sketchybar_role_dev__159_["<policy:sketchybar-role-dev>[159]"]:::_policy_sketchybar_role_dev__159__c
  _policy_sketchybar_role_gui__160_["<policy:sketchybar-role-gui>[160]"]:::_policy_sketchybar_role_gui__160__c
  _policy_sketchybar_role_study__161_["<policy:sketchybar-role-study>[161]"]:::_policy_sketchybar_role_study__161__c
  _policy_sketchybar_app_font_role_dev__162_["<policy:sketchybar_app_font-role-dev>[162]"]:::_policy_sketchybar_app_font_role_dev__162__c
  _policy_sketchybar_app_font_role_gui__163_["<policy:sketchybar_app_font-role-gui>[163]"]:::_policy_sketchybar_app_font_role_gui__163__c
  _policy_sketchybar_app_font_role_study__164_["<policy:sketchybar_app_font-role-study>[164]"]:::_policy_sketchybar_app_font_role_study__164__c
  _policy_sources_role_dev__165_["<policy:sources-role-dev>[165]"]:::_policy_sources_role_dev__165__c
  _policy_sources_role_gui__166_["<policy:sources-role-gui>[166]"]:::_policy_sources_role_gui__166__c
  _policy_sources_role_study__167_["<policy:sources-role-study>[167]"]:::_policy_sources_role_study__167__c
  _policy_starship_role_dev__168_["<policy:starship-role-dev>[168]"]:::_policy_starship_role_dev__168__c
  _policy_starship_role_gui__169_["<policy:starship-role-gui>[169]"]:::_policy_starship_role_gui__169__c
  _policy_starship_role_study__170_["<policy:starship-role-study>[170]"]:::_policy_starship_role_study__170__c
  _policy_stylix_role_dev__171_["<policy:stylix-role-dev>[171]"]:::_policy_stylix_role_dev__171__c
  _policy_stylix_role_gui__172_["<policy:stylix-role-gui>[172]"]:::_policy_stylix_role_gui__172__c
  _policy_stylix_role_study__173_["<policy:stylix-role-study>[173]"]:::_policy_stylix_role_study__173__c
  _policy_unfree_predicate_role_dev__174_["<policy:unfree-predicate-role-dev>[174]"]:::_policy_unfree_predicate_role_dev__174__c
  _policy_unfree_predicate_role_gui__175_["<policy:unfree-predicate-role-gui>[175]"]:::_policy_unfree_predicate_role_gui__175__c
  _policy_unfree_predicate_role_study__176_["<policy:unfree-predicate-role-study>[176]"]:::_policy_unfree_predicate_role_study__176__c
  _policy_wakatime_role_dev__177_["<policy:wakatime-role-dev>[177]"]:::_policy_wakatime_role_dev__177__c
  _policy_wakatime_role_gui__178_["<policy:wakatime-role-gui>[178]"]:::_policy_wakatime_role_gui__178__c
  _policy_wakatime_role_study__179_["<policy:wakatime-role-study>[179]"]:::_policy_wakatime_role_study__179__c
  _policy_zen_role_dev__180_["<policy:zen-role-dev>[180]"]:::_policy_zen_role_dev__180__c
  _policy_zen_role_gui__181_["<policy:zen-role-gui>[181]"]:::_policy_zen_role_gui__181__c
  _policy_zen_role_study__182_["<policy:zen-role-study>[182]"]:::_policy_zen_role_study__182__c
  _policy_zotero_mcp_role_dev__183_["<policy:zotero-mcp-role-dev>[183]"]:::_policy_zotero_mcp_role_dev__183__c
  _policy_zotero_mcp_role_gui__184_["<policy:zotero-mcp-role-gui>[184]"]:::_policy_zotero_mcp_role_gui__184__c
  _policy_zotero_mcp_role_study__185_["<policy:zotero-mcp-role-study>[185]"]:::_policy_zotero_mcp_role_study__185__c
  agenix_rekey_user_ivypierlot["agenix-rekey"]:::agenix_rekey_user_ivypierlot_c
  agenix_rekey_role_dev["agenix-rekey-role-dev"]:::agenix_rekey_role_dev_c
  agenix_rekey_role_gui["agenix-rekey-role-gui"]:::agenix_rekey_role_gui_c
  agenix_rekey_role_study["agenix-rekey-role-study"]:::agenix_rekey_role_study_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  browsers__zen___when__4___anon__0__to_hosts["browsers/zen/<when>:4/<anon>:0/to-hosts"]:::browsers__zen___when__4___anon__0__to_hosts_c
  celler_push["celler-push"]:::celler_push_c
  celler_push_role_dev["celler-push-role-dev"]:::celler_push_role_dev_c
  celler_push_role_gui["celler-push-role-gui"]:::celler_push_role_gui_c
  celler_push_role_study["celler-push-role-study"]:::celler_push_role_study_c
  packages__cotabby[/"packages/cotabby"\]:::packages__cotabby_c
  cotabby["cotabby"]:::cotabby_c
  cotabby_role_dev["cotabby-role-dev"]:::cotabby_role_dev_c
  cotabby_role_gui["cotabby-role-gui"]:::cotabby_role_gui_c
  cotabby_role_study["cotabby-role-study"]:::cotabby_role_study_c
  default_user_ivypierlot["default"]:::default_user_ivypierlot_c
  default_role_dev["default-role-dev"]:::default_role_dev_c
  default_role_gui["default-role-gui"]:::default_role_gui_c
  default_role_study["default-role-study"]:::default_role_study_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  define_user_role_dev["define-user-role-dev"]:::define_user_role_dev_c
  define_user_role_gui["define-user-role-gui"]:::define_user_role_gui_c
  define_user_role_study["define-user-role-study"]:::define_user_role_study_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_cli_role_dev["dev-cli-role-dev"]:::dev_cli_role_dev_c
  dev_cli_role_gui["dev-cli-role-gui"]:::dev_cli_role_gui_c
  dev_cli_role_study["dev-cli-role-study"]:::dev_cli_role_study_c
  dev_nix["dev-nix"]:::dev_nix_c
  dev_nix_role_dev["dev-nix-role-dev"]:::dev_nix_role_dev_c
  dev_nix_role_gui["dev-nix-role-gui"]:::dev_nix_role_gui_c
  dev_nix_role_study["dev-nix-role-study"]:::dev_nix_role_study_c
  dev_role_dev["dev-role-dev"]:::dev_role_dev_c
  dev_role_gui["dev-role-gui"]:::dev_role_gui_c
  dev_role_study["dev-role-study"]:::dev_role_study_c
  difftastic_role_dev["difftastic-role-dev"]:::difftastic_role_dev_c
  difftastic_role_gui["difftastic-role-gui"]:::difftastic_role_gui_c
  difftastic_role_study["difftastic-role-study"]:::difftastic_role_study_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  eagle_nvim_role_dev["eagle-nvim-role-dev"]:::eagle_nvim_role_dev_c
  eagle_nvim_role_gui["eagle-nvim-role-gui"]:::eagle_nvim_role_gui_c
  eagle_nvim_role_study["eagle-nvim-role-study"]:::eagle_nvim_role_study_c
  extra_registry_role_dev["extra-registry-role-dev"]:::extra_registry_role_dev_c
  extra_registry_role_gui["extra-registry-role-gui"]:::extra_registry_role_gui_c
  extra_registry_role_study["extra-registry-role-study"]:::extra_registry_role_study_c
  file_local["file-local"]:::file_local_c
  file_local_role_dev["file-local-role-dev"]:::file_local_role_dev_c
  file_local_role_gui["file-local-role-gui"]:::file_local_role_gui_c
  file_local_role_study["file-local-role-study"]:::file_local_role_study_c
  fish["fish"]:::fish_c
  fish_role_dev["fish-role-dev"]:::fish_role_dev_c
  fish_role_gui["fish-role-gui"]:::fish_role_gui_c
  fish_role_study["fish-role-study"]:::fish_role_study_c
  fish___anon__4__to_hosts["fish/<anon>:4/to-hosts"]:::fish___anon__4__to_hosts_c
  fonts_user_ivypierlot["fonts"]:::fonts_user_ivypierlot_c
  fonts_role_dev["fonts-role-dev"]:::fonts_role_dev_c
  fonts_role_gui["fonts-role-gui"]:::fonts_role_gui_c
  fonts_role_study["fonts-role-study"]:::fonts_role_study_c
  packages__ghostty[/"packages/ghostty"\]:::packages__ghostty_c
  ghostty["ghostty"]:::ghostty_c
  ghostty_role_dev["ghostty-role-dev"]:::ghostty_role_dev_c
  ghostty_role_gui["ghostty-role-gui"]:::ghostty_role_gui_c
  ghostty_role_study["ghostty-role-study"]:::ghostty_role_study_c
  gpg["gpg"]:::gpg_c
  gpg_role_dev["gpg-role-dev"]:::gpg_role_dev_c
  gpg_role_gui["gpg-role-gui"]:::gpg_role_gui_c
  gpg_role_study["gpg-role-study"]:::gpg_role_study_c
  gui["gui"]:::gui_c
  gui_role_dev["gui-role-dev"]:::gui_role_dev_c
  gui_role_gui["gui-role-gui"]:::gui_role_gui_c
  gui_role_study["gui-role-study"]:::gui_role_study_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  home_base_role_dev["home-base-role-dev"]:::home_base_role_dev_c
  home_base_role_gui["home-base-role-gui"]:::home_base_role_gui_c
  home_base_role_study["home-base-role-study"]:::home_base_role_study_c
  homebrew_user_ivypierlot["homebrew"]:::homebrew_user_ivypierlot_c
  homebrew_role_dev["homebrew-role-dev"]:::homebrew_role_dev_c
  homebrew_role_gui["homebrew-role-gui"]:::homebrew_role_gui_c
  homebrew_role_study["homebrew-role-study"]:::homebrew_role_study_c
  hostname_role_dev["hostname-role-dev"]:::hostname_role_dev_c
  hostname_role_gui["hostname-role-gui"]:::hostname_role_gui_c
  hostname_role_study["hostname-role-study"]:::hostname_role_study_c
  idris["idris"]:::idris_c
  idris_role_dev["idris-role-dev"]:::idris_role_dev_c
  idris_role_gui["idris-role-gui"]:::idris_role_gui_c
  idris_role_study["idris-role-study"]:::idris_role_study_c
  inputs__role_dev["inputs'-role-dev"]:::inputs__role_dev_c
  inputs__role_gui["inputs'-role-gui"]:::inputs__role_gui_c
  inputs__role_study["inputs'-role-study"]:::inputs__role_study_c
  insecure_predicate_role_dev["insecure-predicate-role-dev"]:::insecure_predicate_role_dev_c
  insecure_predicate_role_gui["insecure-predicate-role-gui"]:::insecure_predicate_role_gui_c
  insecure_predicate_role_study["insecure-predicate-role-study"]:::insecure_predicate_role_study_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy_fetch_role_dev["ivy-fetch-role-dev"]:::ivy_fetch_role_dev_c
  ivy_fetch_role_gui["ivy-fetch-role-gui"]:::ivy_fetch_role_gui_c
  ivy_fetch_role_study["ivy-fetch-role-study"]:::ivy_fetch_role_study_c
  ivypierlot{{"ivypierlot"}}:::ivypierlot_c
  ivypierlot_role_dev["ivypierlot-role-dev"]:::ivypierlot_role_dev_c
  ivypierlot_role_gui["ivypierlot-role-gui"]:::ivypierlot_role_gui_c
  ivypierlot_role_study["ivypierlot-role-study"]:::ivypierlot_role_study_c
  ivypierlot__Ivys_MacBook_Pro["ivypierlot/Ivys-MacBook-Pro"]:::ivypierlot__Ivys_MacBook_Pro_c
  packages__jankyborders[/"packages/jankyborders"\]:::packages__jankyborders_c
  jankyborders["jankyborders"]:::jankyborders_c
  jankyborders_role_dev["jankyborders-role-dev"]:::jankyborders_role_dev_c
  jankyborders_role_gui["jankyborders-role-gui"]:::jankyborders_role_gui_c
  jankyborders_role_study["jankyborders-role-study"]:::jankyborders_role_study_c
  packages__jj_mcp_server[/"packages/jj-mcp-server"\]:::packages__jj_mcp_server_c
  jj_mcp_server_role_dev["jj-mcp-server-role-dev"]:::jj_mcp_server_role_dev_c
  jj_mcp_server_role_gui["jj-mcp-server-role-gui"]:::jj_mcp_server_role_gui_c
  jj_mcp_server_role_study["jj-mcp-server-role-study"]:::jj_mcp_server_role_study_c
  jujutsu_role_dev["jujutsu-role-dev"]:::jujutsu_role_dev_c
  jujutsu_role_gui["jujutsu-role-gui"]:::jujutsu_role_gui_c
  jujutsu_role_study["jujutsu-role-study"]:::jujutsu_role_study_c
  kanata["kanata"]:::kanata_c
  kanata_role_dev["kanata-role-dev"]:::kanata_role_dev_c
  kanata_role_gui["kanata-role-gui"]:::kanata_role_gui_c
  kanata_role_study["kanata-role-study"]:::kanata_role_study_c
  packages__kanata_tray[/"packages/kanata-tray"\]:::packages__kanata_tray_c
  kanata___anon__4__to_hosts["kanata/<anon>:4/to-hosts"]:::kanata___anon__4__to_hosts_c
  kind_system_routes_user_ivypierlot["kind-system-routes"]:::kind_system_routes_user_ivypierlot_c
  laptop_brew["laptop-brew"]:::laptop_brew_c
  lib_role_dev["lib-role-dev"]:::lib_role_dev_c
  lib_role_gui["lib-role-gui"]:::lib_role_gui_c
  lib_role_study["lib-role-study"]:::lib_role_study_c
  llama_cpp["llama-cpp"]:::llama_cpp_c
  llama_cpp_role_dev["llama-cpp-role-dev"]:::llama_cpp_role_dev_c
  llama_cpp_role_gui["llama-cpp-role-gui"]:::llama_cpp_role_gui_c
  llama_cpp_role_study["llama-cpp-role-study"]:::llama_cpp_role_study_c
  lsp_servers_to_homeManager["lsp-servers-to-homeManager"]:::lsp_servers_to_homeManager_c
  lsp_servers_to_nvim["lsp-servers-to-nvim"]:::lsp_servers_to_nvim_c
  packages__lspmux[/"packages/lspmux"\]:::packages__lspmux_c
  lspmux["lspmux"]:::lspmux_c
  lspmux_role_dev["lspmux-role-dev"]:::lspmux_role_dev_c
  lspmux_role_gui["lspmux-role-gui"]:::lspmux_role_gui_c
  lspmux_role_study["lspmux-role-study"]:::lspmux_role_study_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key_role_dev["main-ssh-key-role-dev"]:::main_ssh_key_role_dev_c
  main_ssh_key_role_gui["main-ssh-key-role-gui"]:::main_ssh_key_role_gui_c
  main_ssh_key_role_study["main-ssh-key-role-study"]:::main_ssh_key_role_study_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  mcp_servers["mcp-servers"]:::mcp_servers_c
  mcp_servers_role_dev["mcp-servers-role-dev"]:::mcp_servers_role_dev_c
  mcp_servers_role_gui["mcp-servers-role-gui"]:::mcp_servers_role_gui_c
  mcp_servers_role_study["mcp-servers-role-study"]:::mcp_servers_role_study_c
  neovim["neovim"]:::neovim_c
  neovim_role_dev["neovim-role-dev"]:::neovim_role_dev_c
  neovim_role_gui["neovim-role-gui"]:::neovim_role_gui_c
  neovim_role_study["neovim-role-study"]:::neovim_role_study_c
  neovim__to_users["neovim/to-users"]:::neovim__to_users_c
  nh_env["nh-env"]:::nh_env_c
  nix_index_role_dev["nix-index-role-dev"]:::nix_index_role_dev_c
  nix_index_role_gui["nix-index-role-gui"]:::nix_index_role_gui_c
  nix_index_role_study["nix-index-role-study"]:::nix_index_role_study_c
  nix_role_dev["nix-role-dev"]:::nix_role_dev_c
  nix_role_gui["nix-role-gui"]:::nix_role_gui_c
  nix_role_study["nix-role-study"]:::nix_role_study_c
  nix_to_host_user_ivypierlot["nix-to-host"]:::nix_to_host_user_ivypierlot_c
  nixpkgs_config_user_ivypierlot["nixpkgs-config"]:::nixpkgs_config_user_ivypierlot_c
  nixpkgs_config_role_dev["nixpkgs-config-role-dev"]:::nixpkgs_config_role_dev_c
  nixpkgs_config_role_gui["nixpkgs-config-role-gui"]:::nixpkgs_config_role_gui_c
  nixpkgs_config_role_study["nixpkgs-config-role-study"]:::nixpkgs_config_role_study_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_ivypierlot["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_ivypierlot_c
  nixvim_role_dev["nixvim-role-dev"]:::nixvim_role_dev_c
  nixvim_role_gui["nixvim-role-gui"]:::nixvim_role_gui_c
  nixvim_role_study["nixvim-role-study"]:::nixvim_role_study_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  nushell["nushell"]:::nushell_c
  nushell_role_dev["nushell-role-dev"]:::nushell_role_dev_c
  nushell_role_gui["nushell-role-gui"]:::nushell_role_gui_c
  nushell_role_study["nushell-role-study"]:::nushell_role_study_c
  onepassword["onepassword"]:::onepassword_c
  onepassword_role_dev["onepassword-role-dev"]:::onepassword_role_dev_c
  onepassword_role_gui["onepassword-role-gui"]:::onepassword_role_gui_c
  onepassword_role_study["onepassword-role-study"]:::onepassword_role_study_c
  onepassword___when__5["onepassword/<when>:5"]:::onepassword___when__5_c
  openclaw["openclaw"]:::openclaw_c
  openclaw_role_dev["openclaw-role-dev"]:::openclaw_role_dev_c
  openclaw_role_gui["openclaw-role-gui"]:::openclaw_role_gui_c
  openclaw_role_study["openclaw-role-study"]:::openclaw_role_study_c
  opencode["opencode"]:::opencode_c
  opencode_role_dev["opencode-role-dev"]:::opencode_role_dev_c
  opencode_role_gui["opencode-role-gui"]:::opencode_role_gui_c
  opencode_role_study["opencode-role-study"]:::opencode_role_study_c
  os_to_host_user_ivypierlot["os-to-host"]:::os_to_host_user_ivypierlot_c
  overlays_role_dev["overlays-role-dev"]:::overlays_role_dev_c
  overlays_role_gui["overlays-role-gui"]:::overlays_role_gui_c
  overlays_role_study["overlays-role-study"]:::overlays_role_study_c
  overlays_to__overlays_user_ivypierlot["overlays-to-_overlays"]:::overlays_to__overlays_user_ivypierlot_c
  overlays_to_flake_parts_user_ivypierlot["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_ivypierlot_c
  pam_rssh_user_ivypierlot["pam-rssh"]:::pam_rssh_user_ivypierlot_c
  pam_rssh_role_dev["pam-rssh-role-dev"]:::pam_rssh_role_dev_c
  pam_rssh_role_gui["pam-rssh-role-gui"]:::pam_rssh_role_gui_c
  pam_rssh_role_study["pam-rssh-role-study"]:::pam_rssh_role_study_c
  pipe_unfree_user_ivypierlot["pipe-unfree"]:::pipe_unfree_user_ivypierlot_c
  den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro_{{"batteries/primary-user(ivypierlot@Ivys-MacBook-Pro)"}}:::den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro__c
  rift["rift"]:::rift_c
  rift_role_dev["rift-role-dev"]:::rift_role_dev_c
  rift_role_gui["rift-role-gui"]:::rift_role_gui_c
  rift_role_study["rift-role-study"]:::rift_role_study_c
  route_casks_user_ivypierlot["route-casks"]:::route_casks_user_ivypierlot_c
  rust["rust"]:::rust_c
  rust_role_dev["rust-role-dev"]:::rust_role_dev_c
  rust_role_gui["rust-role-gui"]:::rust_role_gui_c
  rust_role_study["rust-role-study"]:::rust_role_study_c
  self__role_dev["self'-role-dev"]:::self__role_dev_c
  self__role_gui["self'-role-gui"]:::self__role_gui_c
  self__role_study["self'-role-study"]:::self__role_study_c
  shell_user_ivypierlot["shell"]:::shell_user_ivypierlot_c
  shell_role_dev["shell-role-dev"]:::shell_role_dev_c
  shell_role_gui["shell-role-gui"]:::shell_role_gui_c
  shell_role_study["shell-role-study"]:::shell_role_study_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  packages__sketchybar[/"packages/sketchybar"\]:::packages__sketchybar_c
  sketchybar["sketchybar"]:::sketchybar_c
  sketchybar_role_dev["sketchybar-role-dev"]:::sketchybar_role_dev_c
  sketchybar_role_gui["sketchybar-role-gui"]:::sketchybar_role_gui_c
  sketchybar_role_study["sketchybar-role-study"]:::sketchybar_role_study_c
  packages__sketchybar_app_font[/"packages/sketchybar_app_font"\]:::packages__sketchybar_app_font_c
  sketchybar_app_font_role_dev["sketchybar_app_font-role-dev"]:::sketchybar_app_font_role_dev_c
  sketchybar_app_font_role_gui["sketchybar_app_font-role-gui"]:::sketchybar_app_font_role_gui_c
  sketchybar_app_font_role_study["sketchybar_app_font-role-study"]:::sketchybar_app_font_role_study_c
  sources_role_dev["sources-role-dev"]:::sources_role_dev_c
  sources_role_gui["sources-role-gui"]:::sources_role_gui_c
  sources_role_study["sources-role-study"]:::sources_role_study_c
  starship_role_dev["starship-role-dev"]:::starship_role_dev_c
  starship_role_gui["starship-role-gui"]:::starship_role_gui_c
  starship_role_study["starship-role-study"]:::starship_role_study_c
  stylix["stylix"]:::stylix_c
  stylix_role_dev["stylix-role-dev"]:::stylix_role_dev_c
  stylix_role_gui["stylix-role-gui"]:::stylix_role_gui_c
  stylix_role_study["stylix-role-study"]:::stylix_role_study_c
  sudoagents_user_ivypierlot["sudoagents"]:::sudoagents_user_ivypierlot_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  den__provides__unfree_libkey_nomad_onepassword_password_manager_{{"provides/unfree(libkey-nomad,onepassword-password-manager)"}}:::den__provides__unfree_libkey_nomad_onepassword_password_manager__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  unfree_predicate_role_dev["unfree-predicate-role-dev"]:::unfree_predicate_role_dev_c
  unfree_predicate_role_gui["unfree-predicate-role-gui"]:::unfree_predicate_role_gui_c
  unfree_predicate_role_study["unfree-predicate-role-study"]:::unfree_predicate_role_study_c
  user["user"]:::user_c
  user_shell__ivypierlot_Ivys_MacBook_Pro{{"user-shell/ivypierlot@Ivys-MacBook-Pro"}}:::user_shell__ivypierlot_Ivys_MacBook_Pro_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  user__resolve__when__4_["user/resolve(<when>:4)"]:::user__resolve__when__4__c
  user__resolve__when__5_["user/resolve(<when>:5)"]:::user__resolve__when__5__c
  user__resolve_dev_nix_{{"user/resolve(dev-nix)"}}:::user__resolve_dev_nix__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_jankyborders_{{"user/resolve(jankyborders)"}}:::user__resolve_jankyborders__c
  user__resolve_kanata_{{"user/resolve(kanata)"}}:::user__resolve_kanata__c
  user__resolve_lspmux_{{"user/resolve(lspmux)"}}:::user__resolve_lspmux__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  user__resolve_zen_{{"user/resolve(zen)"}}:::user__resolve_zen__c
  wakatime["wakatime"]:::wakatime_c
  wakatime_role_dev["wakatime-role-dev"]:::wakatime_role_dev_c
  wakatime_role_gui["wakatime-role-gui"]:::wakatime_role_gui_c
  wakatime_role_study["wakatime-role-study"]:::wakatime_role_study_c
  browsers__zen[/"browsers/zen"\]:::browsers__zen_c
  zen_classes["zen-classes"]:::zen_classes_c
  zen_role_dev["zen-role-dev"]:::zen_role_dev_c
  zen_role_gui["zen-role-gui"]:::zen_role_gui_c
  zen_role_study["zen-role-study"]:::zen_role_study_c
  zotero["zotero"]:::zotero_c
  packages__zotero_mcp[/"packages/zotero-mcp"\]:::packages__zotero_mcp_c
  zotero_mcp_role_dev["zotero-mcp-role-dev"]:::zotero_mcp_role_dev_c
  zotero_mcp_role_gui["zotero-mcp-role-gui"]:::zotero_mcp_role_gui_c
  zotero_mcp_role_study["zotero-mcp-role-study"]:::zotero_mcp_role_study_c
  _policy_ivypierlot_role_study__80_ --> zotero
  _policy_zen_role_study__182_ --> zen_classes
  browsers__zen --> den__provides__unfree_libkey_nomad_onepassword_password_manager_
  browsers__zen --> user__resolve_zen_
  cotabby --> packages__cotabby
  dev --> dev_cli
  dev --> dev_nix
  dev_nix --> user__resolve_dev_nix_
  fish --> shell_user_ivypierlot
  fish --> user_shell__ivypierlot_Ivys_MacBook_Pro
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ghostty --> packages__ghostty
  idris --> lspmux
  ivy_fetch --> packages__ivy_fetch
  ivypierlot --> agenix_rekey_user_ivypierlot
  ivypierlot --> celler_push
  ivypierlot --> cotabby
  ivypierlot --> dev
  ivypierlot --> file_local
  ivypierlot --> fish
  ivypierlot --> ghostty
  ivypierlot --> gpg
  ivypierlot --> gui
  ivypierlot --> homebrew_user_ivypierlot
  ivypierlot --> idris
  ivypierlot --> kanata
  ivypierlot --> llama_cpp
  ivypierlot --> neovim
  ivypierlot --> nixvim
  ivypierlot --> nushell
  ivypierlot --> onepassword
  ivypierlot --> openclaw
  ivypierlot --> opencode
  ivypierlot --> den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro_
  ivypierlot --> rift
  ivypierlot --> sketchybar
  ivypierlot --> browsers__zen
  ivypierlot__Ivys_MacBook_Pro --> laptop_brew
  jankyborders --> packages__jankyborders
  jankyborders --> user__resolve_jankyborders_
  kanata --> packages__kanata_tray
  kanata --> user__resolve_kanata_
  lspmux --> packages__lspmux
  lspmux --> user__resolve__anon__4_
  lspmux --> user__resolve_lspmux_
  main_ssh_key --> pam_rssh_user_ivypierlot
  mcp_servers --> packages__jj_mcp_server
  mcp_servers --> packages__zotero_mcp
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> rust
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  onepassword --> onepassword___when__5
  onepassword --> den__provides__unfree_onepassword_password_manager_
  onepassword___when__5 --> user__resolve__when__5_
  opencode --> mcp_servers
  rift --> jankyborders
  sketchybar --> packages__sketchybar
  sketchybar --> packages__sketchybar_app_font
  user --> _policy_agenix_rekey_role_dev__0_
  user --> _policy_agenix_rekey_role_gui__1_
  user --> _policy_agenix_rekey_role_study__2_
  user --> _policy_agenix_rekey__to_users__3_
  user --> _policy_celler_push_role_dev__5_
  user --> _policy_celler_push_role_gui__6_
  user --> _policy_celler_push_role_study__7_
  user --> _policy_cotabby_role_dev__8_
  user --> _policy_cotabby_role_gui__9_
  user --> _policy_cotabby_role_study__10_
  user --> _policy_default_role_dev__11_
  user --> _policy_default_role_gui__12_
  user --> _policy_default_role_study__13_
  user --> _policy_default__to_hosts__14_
  user --> _policy_define_user_role_dev__15_
  user --> _policy_define_user_role_gui__16_
  user --> _policy_define_user_role_study__17_
  user --> _policy_dev_cli_role_dev__18_
  user --> _policy_dev_cli_role_gui__19_
  user --> _policy_dev_cli_role_study__20_
  user --> _policy_dev_nix_role_dev__21_
  user --> _policy_dev_nix_role_gui__22_
  user --> _policy_dev_nix_role_study__23_
  user --> _policy_dev_role_dev__24_
  user --> _policy_dev_role_gui__25_
  user --> _policy_dev_role_study__26_
  user --> _policy_difftastic_role_dev__27_
  user --> _policy_difftastic_role_gui__28_
  user --> _policy_difftastic_role_study__29_
  user --> _policy_eagle_nvim_role_dev__30_
  user --> _policy_eagle_nvim_role_gui__31_
  user --> _policy_eagle_nvim_role_study__32_
  user --> _policy_extra_registry_role_dev__33_
  user --> _policy_extra_registry_role_gui__34_
  user --> _policy_extra_registry_role_study__35_
  user --> _policy_file_local_role_dev__36_
  user --> _policy_file_local_role_gui__37_
  user --> _policy_file_local_role_study__38_
  user --> _policy_fish_role_dev__39_
  user --> _policy_fish_role_gui__40_
  user --> _policy_fish_role_study__41_
  user --> _policy_fonts_role_dev__43_
  user --> _policy_fonts_role_gui__44_
  user --> _policy_fonts_role_study__45_
  user --> _policy_ghostty_role_dev__46_
  user --> _policy_ghostty_role_gui__47_
  user --> _policy_ghostty_role_study__48_
  user --> _policy_gpg_role_dev__49_
  user --> _policy_gpg_role_gui__50_
  user --> _policy_gpg_role_study__51_
  user --> _policy_gui_role_dev__52_
  user --> _policy_gui_role_gui__53_
  user --> _policy_gui_role_study__54_
  user --> _policy_hm_user_detect__55_
  user --> _policy_home_base_role_dev__57_
  user --> _policy_home_base_role_gui__58_
  user --> _policy_home_base_role_study__59_
  user --> _policy_homebrew_role_dev__60_
  user --> _policy_homebrew_role_gui__61_
  user --> _policy_homebrew_role_study__62_
  user --> _policy_hostname_role_dev__63_
  user --> _policy_hostname_role_gui__64_
  user --> _policy_hostname_role_study__65_
  user --> _policy_idris_role_dev__66_
  user --> _policy_idris_role_gui__67_
  user --> _policy_idris_role_study__68_
  user --> _policy_inputs__role_dev__69_
  user --> _policy_inputs__role_gui__70_
  user --> _policy_inputs__role_study__71_
  user --> _policy_insecure_predicate_role_dev__72_
  user --> _policy_insecure_predicate_role_gui__73_
  user --> _policy_insecure_predicate_role_study__74_
  user --> _policy_ivy_fetch_role_dev__75_
  user --> _policy_ivy_fetch_role_gui__76_
  user --> _policy_ivy_fetch_role_study__77_
  user --> _policy_ivypierlot_role_dev__78_
  user --> _policy_ivypierlot_role_gui__79_
  user --> _policy_ivypierlot_role_study__80_
  user --> _policy_jankyborders_role_dev__82_
  user --> _policy_jankyborders_role_gui__83_
  user --> _policy_jankyborders_role_study__84_
  user --> _policy_jj_mcp_server_role_dev__85_
  user --> _policy_jj_mcp_server_role_gui__86_
  user --> _policy_jj_mcp_server_role_study__87_
  user --> _policy_jujutsu_role_dev__88_
  user --> _policy_jujutsu_role_gui__89_
  user --> _policy_jujutsu_role_study__90_
  user --> _policy_kanata_role_dev__91_
  user --> _policy_kanata_role_gui__92_
  user --> _policy_kanata_role_study__93_
  user --> _policy_lib_role_dev__95_
  user --> _policy_lib_role_gui__96_
  user --> _policy_lib_role_study__97_
  user --> _policy_llama_cpp_role_dev__98_
  user --> _policy_llama_cpp_role_gui__99_
  user --> _policy_llama_cpp_role_study__100_
  user --> _policy_lspmux_role_dev__101_
  user --> _policy_lspmux_role_gui__102_
  user --> _policy_lspmux_role_study__103_
  user --> _policy_main_ssh_key_role_dev__104_
  user --> _policy_main_ssh_key_role_gui__105_
  user --> _policy_main_ssh_key_role_study__106_
  user --> _policy_main_ssh_key__to_hosts__107_
  user --> _policy_mcp_servers_role_dev__108_
  user --> _policy_mcp_servers_role_gui__109_
  user --> _policy_mcp_servers_role_study__110_
  user --> _policy_neovim_role_dev__111_
  user --> _policy_neovim_role_gui__112_
  user --> _policy_neovim_role_study__113_
  user --> _policy_nix_index_role_dev__115_
  user --> _policy_nix_index_role_gui__116_
  user --> _policy_nix_index_role_study__117_
  user --> _policy_nix_role_dev__118_
  user --> _policy_nix_role_gui__119_
  user --> _policy_nix_role_study__120_
  user --> _policy_nixpkgs_config_role_dev__121_
  user --> _policy_nixpkgs_config_role_gui__122_
  user --> _policy_nixpkgs_config_role_study__123_
  user --> _policy_nixvim_role_dev__124_
  user --> _policy_nixvim_role_gui__125_
  user --> _policy_nixvim_role_study__126_
  user --> _policy_nixvim_user_forward__127_
  user --> _policy_nushell_role_dev__128_
  user --> _policy_nushell_role_gui__129_
  user --> _policy_nushell_role_study__130_
  user --> _policy_onepassword_role_dev__131_
  user --> _policy_onepassword_role_gui__132_
  user --> _policy_onepassword_role_study__133_
  user --> _policy_openclaw_role_dev__134_
  user --> _policy_openclaw_role_gui__135_
  user --> _policy_openclaw_role_study__136_
  user --> _policy_opencode_role_dev__137_
  user --> _policy_opencode_role_gui__138_
  user --> _policy_opencode_role_study__139_
  user --> _policy_overlays_role_dev__140_
  user --> _policy_overlays_role_gui__141_
  user --> _policy_overlays_role_study__142_
  user --> _policy_pam_rssh_role_dev__143_
  user --> _policy_pam_rssh_role_gui__144_
  user --> _policy_pam_rssh_role_study__145_
  user --> _policy_rift_role_dev__146_
  user --> _policy_rift_role_gui__147_
  user --> _policy_rift_role_study__148_
  user --> _policy_rust_role_dev__149_
  user --> _policy_rust_role_gui__150_
  user --> _policy_rust_role_study__151_
  user --> _policy_self__role_dev__152_
  user --> _policy_self__role_gui__153_
  user --> _policy_self__role_study__154_
  user --> _policy_shell_role_dev__155_
  user --> _policy_shell_role_gui__156_
  user --> _policy_shell_role_study__157_
  user --> _policy_sketchybar_role_dev__159_
  user --> _policy_sketchybar_role_gui__160_
  user --> _policy_sketchybar_role_study__161_
  user --> _policy_sketchybar_app_font_role_dev__162_
  user --> _policy_sketchybar_app_font_role_gui__163_
  user --> _policy_sketchybar_app_font_role_study__164_
  user --> _policy_sources_role_dev__165_
  user --> _policy_sources_role_gui__166_
  user --> _policy_sources_role_study__167_
  user --> _policy_starship_role_dev__168_
  user --> _policy_starship_role_gui__169_
  user --> _policy_starship_role_study__170_
  user --> _policy_stylix_role_dev__171_
  user --> _policy_stylix_role_gui__172_
  user --> _policy_stylix_role_study__173_
  user --> _policy_unfree_predicate_role_dev__174_
  user --> _policy_unfree_predicate_role_gui__175_
  user --> _policy_unfree_predicate_role_study__176_
  user --> _policy_wakatime_role_dev__177_
  user --> _policy_wakatime_role_gui__178_
  user --> _policy_wakatime_role_study__179_
  user --> _policy_zen_role_dev__180_
  user --> _policy_zen_role_gui__181_
  user --> _policy_zen_role_study__182_
  user --> _policy_zotero_mcp_role_dev__183_
  user --> _policy_zotero_mcp_role_gui__184_
  user --> _policy_zotero_mcp_role_study__185_
  user --> ivypierlot__Ivys_MacBook_Pro
  user --> default_user_ivypierlot
  user --> fonts_user_ivypierlot
  user --> ivy_fetch
  user --> ivypierlot
  user --> main_ssh_key
  user --> nixpkgs_config_user_ivypierlot
  user --> sudoagents_user_ivypierlot
  user --> neovim__to_users
  user --> shell__to_users
  user --> user__resolve_user_
  end

  ivypierlot -.->|provides| ivypierlot__Ivys_MacBook_Pro__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy__policy_onepassword_role_gui__132___to_hosts__0__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy__policy_onepassword_role_gui__132___to_users__1__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_Ivys_MacBook_Pro_role_dev__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_Ivys_MacBook_Pro_role_gui__3__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_Ivys_MacBook_Pro_role_study__4__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey_role_dev__0__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey_role_gui__1__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey_role_study__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey__to_users__3__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ccache_role_dev__6__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ccache_role_gui__7__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ccache_role_study__8__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_dev__5__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_gui__6__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_study__7__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_cotabby_role_dev__8__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_cotabby_role_gui__9__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_cotabby_role_study__10__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_dev__10__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_gui__11__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_study__12__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_dev__13__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_gui__14__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_study__15__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_dev__16__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_gui__17__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_study__18__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_dev__19__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_gui__20__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_study__21__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default_role_dev__11__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default_role_gui__12__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default_role_study__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__14__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_dev__15__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_gui__16__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_study__17__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_cli_role_dev__18__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_cli_role_gui__19__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_cli_role_study__20__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_nix_role_dev__21__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_nix_role_gui__22__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_nix_role_study__23__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_role_dev__24__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_role_gui__25__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_role_study__26__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_dev__27__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_gui__28__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_study__29__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_dev__30__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_gui__31__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_study__32__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_dev__33__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_gui__34__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_study__35__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_file_local_role_dev__36__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_file_local_role_gui__37__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_file_local_role_study__38__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_dev__39__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_gui__40__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_study__41__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_dev__43__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_gui__44__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_study__45__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ghostty_role_dev__46__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ghostty_role_gui__47__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ghostty_role_study__48__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_dev__49__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_gui__50__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_study__51__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gui_role_dev__52__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gui_role_gui__53__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gui_role_study__54__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__55__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_dev__57__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_gui__58__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_study__59__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_homebrew_role_dev__60__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_homebrew_role_gui__61__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_homebrew_role_study__62__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_dev__63__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_gui__64__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_study__65__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_idris_role_dev__66__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_idris_role_gui__67__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_idris_role_study__68__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_dev__69__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_gui__70__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_study__71__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_dev__72__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_gui__73__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_study__74__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_dev__75__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_gui__76__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_study__77__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivypierlot_role_dev__78__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivypierlot_role_gui__79__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivypierlot_role_study__80__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jankyborders_role_dev__82__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jankyborders_role_gui__83__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jankyborders_role_study__84__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jj_mcp_server_role_dev__85__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jj_mcp_server_role_gui__86__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jj_mcp_server_role_study__87__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_dev__88__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_gui__89__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_study__90__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_kanata_role_dev__91__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_kanata_role_gui__92__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_kanata_role_study__93__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_karabiner_driver_role_dev__22__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_karabiner_driver_role_gui__23__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_karabiner_driver_role_study__24__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_laptop_brew_role_dev__25__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_laptop_brew_role_gui__26__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_laptop_brew_role_study__27__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_dev__95__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_gui__96__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_study__97__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lix_role_dev__28__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lix_role_gui__29__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lix_role_study__30__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_llama_cpp_role_dev__98__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_llama_cpp_role_gui__99__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_llama_cpp_role_study__100__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_dev__101__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_gui__102__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_study__103__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_dev__104__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_gui__105__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_study__106__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__107__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_mcp_servers_role_dev__108__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_mcp_servers_role_gui__109__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_mcp_servers_role_study__110__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_neovim_role_dev__111__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_neovim_role_gui__112__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_neovim_role_study__113__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_dev__115__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_gui__116__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_study__117__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_dev__118__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_gui__119__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_study__120__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_dev__31__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_gui__32__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_study__33__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_dev__121__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_gui__122__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_study__123__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_dev__124__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_gui__125__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_study__126__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_user_forward__127__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nushell_role_dev__128__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nushell_role_gui__129__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nushell_role_study__130__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_onepassword_role_dev__131__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_onepassword_role_gui__132__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_onepassword_role_study__133__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openclaw_role_dev__134__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openclaw_role_gui__135__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openclaw_role_study__136__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_opencode_role_dev__137__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_opencode_role_gui__138__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_opencode_role_study__139__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_dev__34__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_gui__35__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_study__36__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_dev__140__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_gui__141__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_study__142__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_dev__143__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_gui__144__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_study__145__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_dev__37__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_gui__38__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_study__39__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rift_role_dev__146__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rift_role_gui__147__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rift_role_study__148__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_dev__149__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_gui__150__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_study__151__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sccache_role_dev__40__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sccache_role_gui__41__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sccache_role_study__42__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_dev__152__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_gui__153__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_study__154__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_dev__155__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_gui__156__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_study__157__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sketchybar_role_dev__159__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sketchybar_role_gui__160__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sketchybar_role_study__161__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sketchybar_app_font_role_dev__162__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sketchybar_app_font_role_gui__163__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sketchybar_app_font_role_study__164__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_dev__165__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_gui__166__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_study__167__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_dev__168__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_gui__169__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_study__170__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_dev__171__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_gui__172__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_study__173__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sudoagents_role_dev__43__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sudoagents_role_gui__44__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sudoagents_role_study__45__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_dev__46__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_gui__47__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_study__48__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_dev__174__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_gui__175__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_study__176__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_role_dev__49__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_role_gui__50__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_role_study__51__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_secrets_role_dev__52__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_secrets_role_gui__53__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_secrets_role_study__54__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config_role_dev__55__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config_role_gui__56__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config_role_study__57__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config__to_users__58__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_dev__177__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_gui__178__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_study__179__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zen_classes_role_dev__59__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zen_classes_role_gui__60__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zen_classes_role_study__61__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zen_role_dev__180__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zen_role_gui__181__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zen_role_study__182__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zotero_mcp_role_dev__183__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zotero_mcp_role_gui__184__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zotero_mcp_role_study__185__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zotero_role_dev__62__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zotero_role_gui__63__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zotero_role_study__64__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef ccache__Ivys_MacBook_Pro_c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef browsers_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef browsers__zen___when__4___anon__0__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ccache_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef celler_push_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef celler_push_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef celler_push_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__cotabby_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef cotabby_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef cotabby_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef cotabby_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef cotabby_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef define_user_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef define_user_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef define_user_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_cli_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_cli_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_nix_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_nix_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef difftastic_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef difftastic_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef difftastic_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef eagle_nvim_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef eagle_nvim_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef eagle_nvim_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef extra_registry_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef extra_registry_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef extra_registry_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef file_local_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef file_local_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef file_local_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef file_local_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fish_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish___anon__4__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef packages__ghostty_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ghostty_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ghostty_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ghostty_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef gpg_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef gpg_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gui_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef gui_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef gui_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef home_base_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef homebrew_host_Ivys_MacBook_Pro_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef homebrew_user_ivypierlot_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef homebrew_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef homebrew_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef homebrew_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_vpn__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef hostname_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef hostname_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef hostname_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef idris_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef idris_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef idris_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef idris_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef inputs__role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef inputs__role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef inputs__role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivy_fetch_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivy_fetch_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivypierlot_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivypierlot_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivypierlot_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivypierlot__Ivys_MacBook_Pro_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__jankyborders_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef jankyborders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jankyborders_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jankyborders_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jankyborders_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__jj_mcp_server_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef jj_mcp_server_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jj_mcp_server_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jj_mcp_server_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kanata_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef kanata_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kanata_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kanata_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__kanata_tray_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef kanata___anon__4__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef karabiner_driver_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_Ivys_MacBook_Pro_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_ivypierlot_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef laptop_brew_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef llama_cpp_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef llama_cpp_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef llama_cpp_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef llama_cpp_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lsp_servers_to_homeManager_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lsp_servers_to_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lspmux_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lspmux_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lspmux_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef mcp_servers_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef mcp_servers_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef mcp_servers_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef mcp_servers_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim__to_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_index_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_index_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_index_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__5_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixpkgs_config_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixpkgs_config_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nushell_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nushell_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nushell_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef onepassword_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef onepassword_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef onepassword_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef onepassword___when__5_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef openclaw_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef openclaw_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openclaw_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openclaw_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef opencode_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef opencode_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef opencode_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef opencode_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_rssh_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_rssh_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_rssh_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef rift_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef rift_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rift_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rift_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef rust_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sccache_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef self__role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef self__role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef self__role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__sketchybar_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef sketchybar_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sketchybar_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sketchybar_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sketchybar_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__sketchybar_app_font_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sketchybar_app_font_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sketchybar_app_font_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sketchybar_app_font_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef sources_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sources_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sources_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef starship_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef starship_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef starship_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef stylix_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef stylix_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef stylix_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sudoagents_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sudoagents_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef ivypierlot__Ivys_MacBook_Pro__to_users_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_libkey_nomad_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivypierlot_Ivys_MacBook_Pro_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve__when__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve__when__5__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_dev_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_jankyborders__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_kanata__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_lspmux__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_zen__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef vpn_ssh_config_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef wakatime_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef wakatime_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef wakatime_role_study_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef browsers__zen_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef zen_classes_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef zen_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef zen_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef zen_role_study_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef zotero_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef packages__zotero_mcp_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef zotero_mcp_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef zotero_mcp_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef zotero_mcp_role_study_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
style ctx_host_Ivys_MacBook_Pro fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_ivypierlot fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## auspc

**Architecture:** `x86_64-linux`
**Roles:** gui, gaming, dev

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  auspc([auspc]):::root

  subgraph ctx_host_auspc["host: auspc"]
  _anon_["<anon>"]:::_anon__c
  _policy_alx_wol_role_dev__0_["<policy:alx-wol-role-dev>[0]"]:::_policy_alx_wol_role_dev__0__c
  _policy_alx_wol_role_gaming__1_["<policy:alx-wol-role-gaming>[1]"]:::_policy_alx_wol_role_gaming__1__c
  _policy_alx_wol_role_gui__2_["<policy:alx-wol-role-gui>[2]"]:::_policy_alx_wol_role_gui__2__c
  _policy_auscyber__auspc___anon__6__to_hosts__3_["<policy:auscyber/auspc/<anon>:6/to-hosts>[3]"]:::_policy_auscyber__auspc___anon__6__to_hosts__3__c
  _policy_auspc_role_dev__4_["<policy:auspc-role-dev>[4]"]:::_policy_auspc_role_dev__4__c
  _policy_auspc_role_gaming__5_["<policy:auspc-role-gaming>[5]"]:::_policy_auspc_role_gaming__5__c
  _policy_auspc_role_gui__6_["<policy:auspc-role-gui>[6]"]:::_policy_auspc_role_gui__6__c
  _policy_bootlogo_role_dev__8_["<policy:bootlogo-role-dev>[8]"]:::_policy_bootlogo_role_dev__8__c
  _policy_bootlogo_role_gaming__9_["<policy:bootlogo-role-gaming>[9]"]:::_policy_bootlogo_role_gaming__9__c
  _policy_bootlogo_role_gui__10_["<policy:bootlogo-role-gui>[10]"]:::_policy_bootlogo_role_gui__10__c
  _policy_builder_server_role_dev__11_["<policy:builder-server-role-dev>[11]"]:::_policy_builder_server_role_dev__11__c
  _policy_builder_server_role_gaming__12_["<policy:builder-server-role-gaming>[12]"]:::_policy_builder_server_role_gaming__12__c
  _policy_builder_server_role_gui__13_["<policy:builder-server-role-gui>[13]"]:::_policy_builder_server_role_gui__13__c
  _policy_cachyos_kernel_role_dev__14_["<policy:cachyos-kernel-role-dev>[14]"]:::_policy_cachyos_kernel_role_dev__14__c
  _policy_cachyos_kernel_role_gaming__15_["<policy:cachyos-kernel-role-gaming>[15]"]:::_policy_cachyos_kernel_role_gaming__15__c
  _policy_cachyos_kernel_role_gui__16_["<policy:cachyos-kernel-role-gui>[16]"]:::_policy_cachyos_kernel_role_gui__16__c
  _policy_ccache_role_dev__17_["<policy:ccache-role-dev>[17]"]:::_policy_ccache_role_dev__17__c
  _policy_ccache_role_gaming__18_["<policy:ccache-role-gaming>[18]"]:::_policy_ccache_role_gaming__18__c
  _policy_ccache_role_gui__19_["<policy:ccache-role-gui>[19]"]:::_policy_ccache_role_gui__19__c
  _policy_darwin_base_role_dev__21_["<policy:darwin-base-role-dev>[21]"]:::_policy_darwin_base_role_dev__21__c
  _policy_darwin_base_role_gaming__22_["<policy:darwin-base-role-gaming>[22]"]:::_policy_darwin_base_role_gaming__22__c
  _policy_darwin_base_role_gui__23_["<policy:darwin-base-role-gui>[23]"]:::_policy_darwin_base_role_gui__23__c
  _policy_darwin_finder_role_dev__24_["<policy:darwin-finder-role-dev>[24]"]:::_policy_darwin_finder_role_dev__24__c
  _policy_darwin_finder_role_gaming__25_["<policy:darwin-finder-role-gaming>[25]"]:::_policy_darwin_finder_role_gaming__25__c
  _policy_darwin_finder_role_gui__26_["<policy:darwin-finder-role-gui>[26]"]:::_policy_darwin_finder_role_gui__26__c
  _policy_darwin_general_role_dev__27_["<policy:darwin-general-role-dev>[27]"]:::_policy_darwin_general_role_dev__27__c
  _policy_darwin_general_role_gaming__28_["<policy:darwin-general-role-gaming>[28]"]:::_policy_darwin_general_role_gaming__28__c
  _policy_darwin_general_role_gui__29_["<policy:darwin-general-role-gui>[29]"]:::_policy_darwin_general_role_gui__29__c
  _policy_darwin_hmApps_role_dev__30_["<policy:darwin-hmApps-role-dev>[30]"]:::_policy_darwin_hmApps_role_dev__30__c
  _policy_darwin_hmApps_role_gaming__31_["<policy:darwin-hmApps-role-gaming>[31]"]:::_policy_darwin_hmApps_role_gaming__31__c
  _policy_darwin_hmApps_role_gui__32_["<policy:darwin-hmApps-role-gui>[32]"]:::_policy_darwin_hmApps_role_gui__32__c
  _policy_dev_cli_role_dev__33_["<policy:dev-cli-role-dev>[33]"]:::_policy_dev_cli_role_dev__33__c
  _policy_dev_cli_role_gaming__34_["<policy:dev-cli-role-gaming>[34]"]:::_policy_dev_cli_role_gaming__34__c
  _policy_dev_cli_role_gui__35_["<policy:dev-cli-role-gui>[35]"]:::_policy_dev_cli_role_gui__35__c
  _policy_dev_nix_role_dev__36_["<policy:dev-nix-role-dev>[36]"]:::_policy_dev_nix_role_dev__36__c
  _policy_dev_nix_role_gaming__37_["<policy:dev-nix-role-gaming>[37]"]:::_policy_dev_nix_role_gaming__37__c
  _policy_dev_nix_role_gui__38_["<policy:dev-nix-role-gui>[38]"]:::_policy_dev_nix_role_gui__38__c
  _policy_dev_role_dev__39_["<policy:dev-role-dev>[39]"]:::_policy_dev_role_dev__39__c
  _policy_dev_role_gaming__40_["<policy:dev-role-gaming>[40]"]:::_policy_dev_role_gaming__40__c
  _policy_dev_role_gui__41_["<policy:dev-role-gui>[41]"]:::_policy_dev_role_gui__41__c
  _policy_disko_role_dev__42_["<policy:disko-role-dev>[42]"]:::_policy_disko_role_dev__42__c
  _policy_disko_role_gaming__43_["<policy:disko-role-gaming>[43]"]:::_policy_disko_role_gaming__43__c
  _policy_disko_role_gui__44_["<policy:disko-role-gui>[44]"]:::_policy_disko_role_gui__44__c
  _policy_eagle_nvim_role_dev__45_["<policy:eagle-nvim-role-dev>[45]"]:::_policy_eagle_nvim_role_dev__45__c
  _policy_eagle_nvim_role_gaming__46_["<policy:eagle-nvim-role-gaming>[46]"]:::_policy_eagle_nvim_role_gaming__46__c
  _policy_eagle_nvim_role_gui__47_["<policy:eagle-nvim-role-gui>[47]"]:::_policy_eagle_nvim_role_gui__47__c
  _policy_facter_role_dev__48_["<policy:facter-role-dev>[48]"]:::_policy_facter_role_dev__48__c
  _policy_facter_role_gaming__49_["<policy:facter-role-gaming>[49]"]:::_policy_facter_role_gaming__49__c
  _policy_facter_role_gui__50_["<policy:facter-role-gui>[50]"]:::_policy_facter_role_gui__50__c
  _policy_ghostty_role_dev__51_["<policy:ghostty-role-dev>[51]"]:::_policy_ghostty_role_dev__51__c
  _policy_ghostty_role_gaming__52_["<policy:ghostty-role-gaming>[52]"]:::_policy_ghostty_role_gaming__52__c
  _policy_ghostty_role_gui__53_["<policy:ghostty-role-gui>[53]"]:::_policy_ghostty_role_gui__53__c
  _policy_gpg_role_dev__54_["<policy:gpg-role-dev>[54]"]:::_policy_gpg_role_dev__54__c
  _policy_gpg_role_gaming__55_["<policy:gpg-role-gaming>[55]"]:::_policy_gpg_role_gaming__55__c
  _policy_gpg_role_gui__56_["<policy:gpg-role-gui>[56]"]:::_policy_gpg_role_gui__56__c
  _policy_gpus_role_dev__57_["<policy:gpus-role-dev>[57]"]:::_policy_gpus_role_dev__57__c
  _policy_gpus_role_gaming__58_["<policy:gpus-role-gaming>[58]"]:::_policy_gpus_role_gaming__58__c
  _policy_gpus_role_gui__59_["<policy:gpus-role-gui>[59]"]:::_policy_gpus_role_gui__59__c
  _policy_gui_role_dev__60_["<policy:gui-role-dev>[60]"]:::_policy_gui_role_dev__60__c
  _policy_gui_role_gaming__61_["<policy:gui-role-gaming>[61]"]:::_policy_gui_role_gaming__61__c
  _policy_gui_role_gui__62_["<policy:gui-role-gui>[62]"]:::_policy_gui_role_gui__62__c
  _policy_lspmux_role_dev__63_["<policy:lspmux-role-dev>[63]"]:::_policy_lspmux_role_dev__63__c
  _policy_lspmux_role_gaming__64_["<policy:lspmux-role-gaming>[64]"]:::_policy_lspmux_role_gaming__64__c
  _policy_lspmux_role_gui__65_["<policy:lspmux-role-gui>[65]"]:::_policy_lspmux_role_gui__65__c
  _policy_neovim_role_dev__66_["<policy:neovim-role-dev>[66]"]:::_policy_neovim_role_dev__66__c
  _policy_neovim_role_gaming__67_["<policy:neovim-role-gaming>[67]"]:::_policy_neovim_role_gaming__67__c
  _policy_neovim_role_gui__68_["<policy:neovim-role-gui>[68]"]:::_policy_neovim_role_gui__68__c
  _policy_nixos_general_role_dev__70_["<policy:nixos-general-role-dev>[70]"]:::_policy_nixos_general_role_dev__70__c
  _policy_nixos_general_role_gaming__71_["<policy:nixos-general-role-gaming>[71]"]:::_policy_nixos_general_role_gaming__71__c
  _policy_nixos_general_role_gui__72_["<policy:nixos-general-role-gui>[72]"]:::_policy_nixos_general_role_gui__72__c
  _policy_nixvim_role_dev__73_["<policy:nixvim-role-dev>[73]"]:::_policy_nixvim_role_dev__73__c
  _policy_nixvim_role_gaming__74_["<policy:nixvim-role-gaming>[74]"]:::_policy_nixvim_role_gaming__74__c
  _policy_nixvim_role_gui__75_["<policy:nixvim-role-gui>[75]"]:::_policy_nixvim_role_gui__75__c
  _policy_nushell_role_dev__76_["<policy:nushell-role-dev>[76]"]:::_policy_nushell_role_dev__76__c
  _policy_nushell_role_gaming__77_["<policy:nushell-role-gaming>[77]"]:::_policy_nushell_role_gaming__77__c
  _policy_nushell_role_gui__78_["<policy:nushell-role-gui>[78]"]:::_policy_nushell_role_gui__78__c
  _policy_onepassword_role_dev__79_["<policy:onepassword-role-dev>[79]"]:::_policy_onepassword_role_dev__79__c
  _policy_onepassword_role_gaming__80_["<policy:onepassword-role-gaming>[80]"]:::_policy_onepassword_role_gaming__80__c
  _policy_onepassword_role_gui__81_["<policy:onepassword-role-gui>[81]"]:::_policy_onepassword_role_gui__81__c
  _policy_openssh_role_dev__82_["<policy:openssh-role-dev>[82]"]:::_policy_openssh_role_dev__82__c
  _policy_openssh_role_gaming__83_["<policy:openssh-role-gaming>[83]"]:::_policy_openssh_role_gaming__83__c
  _policy_openssh_role_gui__84_["<policy:openssh-role-gui>[84]"]:::_policy_openssh_role_gui__84__c
  _policy_pam_touchid_role_dev__85_["<policy:pam-touchid-role-dev>[85]"]:::_policy_pam_touchid_role_dev__85__c
  _policy_pam_touchid_role_gaming__86_["<policy:pam-touchid-role-gaming>[86]"]:::_policy_pam_touchid_role_gaming__86__c
  _policy_pam_touchid_role_gui__87_["<policy:pam-touchid-role-gui>[87]"]:::_policy_pam_touchid_role_gui__87__c
  _policy_plasma_role_dev__88_["<policy:plasma-role-dev>[88]"]:::_policy_plasma_role_dev__88__c
  _policy_plasma_role_gaming__89_["<policy:plasma-role-gaming>[89]"]:::_policy_plasma_role_gaming__89__c
  _policy_plasma_role_gui__90_["<policy:plasma-role-gui>[90]"]:::_policy_plasma_role_gui__90__c
  _policy_rust_role_dev__91_["<policy:rust-role-dev>[91]"]:::_policy_rust_role_dev__91__c
  _policy_rust_role_gaming__92_["<policy:rust-role-gaming>[92]"]:::_policy_rust_role_gaming__92__c
  _policy_rust_role_gui__93_["<policy:rust-role-gui>[93]"]:::_policy_rust_role_gui__93__c
  _policy_secure_boot_role_dev__94_["<policy:secure-boot-role-dev>[94]"]:::_policy_secure_boot_role_dev__94__c
  _policy_secure_boot_role_gaming__95_["<policy:secure-boot-role-gaming>[95]"]:::_policy_secure_boot_role_gaming__95__c
  _policy_secure_boot_role_gui__96_["<policy:secure-boot-role-gui>[96]"]:::_policy_secure_boot_role_gui__96__c
  _policy_stylix_role_dev__97_["<policy:stylix-role-dev>[97]"]:::_policy_stylix_role_dev__97__c
  _policy_stylix_role_gaming__98_["<policy:stylix-role-gaming>[98]"]:::_policy_stylix_role_gaming__98__c
  _policy_stylix_role_gui__99_["<policy:stylix-role-gui>[99]"]:::_policy_stylix_role_gui__99__c
  _policy_to_users_role_dev__100_["<policy:to-users-role-dev>[100]"]:::_policy_to_users_role_dev__100__c
  _policy_to_users_role_gaming__101_["<policy:to-users-role-gaming>[101]"]:::_policy_to_users_role_gaming__101__c
  _policy_to_users_role_gui__102_["<policy:to-users-role-gui>[102]"]:::_policy_to_users_role_gui__102__c
  _policy_user_pwd_role_dev__103_["<policy:user-pwd-role-dev>[103]"]:::_policy_user_pwd_role_dev__103__c
  _policy_user_pwd_role_gaming__104_["<policy:user-pwd-role-gaming>[104]"]:::_policy_user_pwd_role_gaming__104__c
  _policy_user_pwd_role_gui__105_["<policy:user-pwd-role-gui>[105]"]:::_policy_user_pwd_role_gui__105__c
  _policy_vpn_role_dev__106_["<policy:vpn-role-dev>[106]"]:::_policy_vpn_role_dev__106__c
  _policy_vpn_role_gaming__107_["<policy:vpn-role-gaming>[107]"]:::_policy_vpn_role_gaming__107__c
  _policy_vpn_role_gui__108_["<policy:vpn-role-gui>[108]"]:::_policy_vpn_role_gui__108__c
  _policy_vpn_secrets_role_dev__109_["<policy:vpn-secrets-role-dev>[109]"]:::_policy_vpn_secrets_role_dev__109__c
  _policy_vpn_secrets_role_gaming__110_["<policy:vpn-secrets-role-gaming>[110]"]:::_policy_vpn_secrets_role_gaming__110__c
  _policy_vpn_secrets_role_gui__111_["<policy:vpn-secrets-role-gui>[111]"]:::_policy_vpn_secrets_role_gui__111__c
  _policy_vpn_ssh_config_role_dev__112_["<policy:vpn-ssh-config-role-dev>[112]"]:::_policy_vpn_ssh_config_role_dev__112__c
  _policy_vpn_ssh_config_role_gaming__113_["<policy:vpn-ssh-config-role-gaming>[113]"]:::_policy_vpn_ssh_config_role_gaming__113__c
  _policy_vpn_ssh_config_role_gui__114_["<policy:vpn-ssh-config-role-gui>[114]"]:::_policy_vpn_ssh_config_role_gui__114__c
  _policy_vpn_ssh_config__to_users__115_["<policy:vpn-ssh-config/to-users>[115]"]:::_policy_vpn_ssh_config__to_users__115__c
  _policy_wakatime_role_dev__116_["<policy:wakatime-role-dev>[116]"]:::_policy_wakatime_role_dev__116__c
  _policy_wakatime_role_gaming__117_["<policy:wakatime-role-gaming>[117]"]:::_policy_wakatime_role_gaming__117__c
  _policy_wakatime_role_gui__118_["<policy:wakatime-role-gui>[118]"]:::_policy_wakatime_role_gui__118__c
  agenix_rekey_host_auspc["agenix-rekey"]:::agenix_rekey_host_auspc_c
  packages__alx_wol[/"packages/alx-wol"\]:::packages__alx_wol_c
  ccache__auspc[/"ccache/auspc"\]:::ccache__auspc_c
  bootlogo["bootlogo"]:::bootlogo_c
  builder_server["builder-server"]:::builder_server_c
  cachyos_kernel["cachyos-kernel"]:::cachyos_kernel_c
  ccache["ccache"]:::ccache_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_auspc["default"]:::default_host_auspc_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__auscyber_auspc{{"batteries/define-user/auscyber@auspc"}}:::den__batteries__define_user__auscyber_auspc_c
  difftastic["difftastic"]:::difftastic_c
  disko["disko"]:::disko_c
  extra_registry["extra-registry"]:::extra_registry_c
  facter["facter"]:::facter_c
  fonts_host_auspc["fonts"]:::fonts_host_auspc_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  gpus["gpus"]:::gpus_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__4_["host/resolve(<when>:4)"]:::host__resolve__when__4__c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_gpus_{{"host/resolve(gpus)"}}:::host__resolve_gpus__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  host__resolve_vpn_{{"host/resolve(vpn)"}}:::host__resolve_vpn__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_auspc["kind-system-routes"]:::kind_system_routes_host_auspc_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_auspc["nix-to-host"]:::nix_to_host_host_auspc_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nix___when__5["nix/<when>:5"]:::nix___when__5_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_auspc["nixpkgs-config"]:::nixpkgs_config_host_auspc_c
  nixvim_include_global_pkgs_host_auspc["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_auspc_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_auspc["os-to-host"]:::os_to_host_host_auspc_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_auspc["overlays-to-_overlays"]:::overlays_to__overlays_host_auspc_c
  overlays_to_flake_parts_host_auspc["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_auspc_c
  pam_rssh_host_auspc["pam-rssh"]:::pam_rssh_host_auspc_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_auspc["pipe-unfree"]:::pipe_unfree_host_auspc_c
  route_casks_host_auspc["route-casks"]:::route_casks_host_auspc_c
  secure_boot["secure-boot"]:::secure_boot_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_auspc["shell"]:::shell_host_auspc_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  auscyber__auspc__to_users[/"auspc/to-users"\]:::auscyber__auspc__to_users_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  den__provides__unfree_castlabs_electron__host_auspc{{"den/provides/unfree(castlabs-electron)"}}:::den__provides__unfree_castlabs_electron__host_auspc_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  vpn_ssh_config["vpn-ssh-config"]:::vpn_ssh_config_c
  auspc --> packages__alx_wol
  auspc --> bootlogo
  auspc --> builder_server
  auspc --> cachyos_kernel
  auspc --> disko
  auspc --> facter
  auspc --> gpus
  auspc --> secure_boot
  auspc --> den__provides__unfree_castlabs_electron__host_auspc
  auspc --> vpn
  cachyos_kernel --> ccache
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_auspc --> den__batteries__define_user
  default_host_auspc --> extra_registry
  default_host_auspc --> home_base
  default_host_auspc --> host__resolve_default_
  default_host_auspc --> den__batteries__hostname
  default_host_auspc --> den__batteries__inputs_
  default_host_auspc --> insecure_predicate
  default_host_auspc --> lib
  default_host_auspc --> nix
  default_host_auspc --> overlays
  default_host_auspc --> den__batteries__self_
  default_host_auspc --> den__batteries__sources
  default_host_auspc --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__auscyber_auspc
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_auspc --> fonts___when__4
  fonts_host_auspc --> fonts___when__5
  gpus --> host__resolve_gpus_
  home_base --> shell_host_auspc
  host --> auspc
  host --> darwin_base
  host --> default_host_auspc
  host --> fonts_host_auspc
  host --> host__resolve_host_
  host --> nixos_general
  host --> nixpkgs_config_host_auspc
  host --> openssh
  host --> pam_rssh_host_auspc
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  nix --> nix___when__4
  nix --> nix___when__5
  nix___when__4 --> host__resolve__when__4_
  nix___when__5 -.-x host__resolve__when__5_
  nixpkgs_config_host_auspc --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_auspc --> jujutsu
  shell_host_auspc --> nix_index
  shell_host_auspc --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> host__resolve_vpn_
  vpn --> vpn_secrets
  vpn --> vpn_ssh_config
  vpn_secrets --> agenix_rekey_host_auspc
  ccache -.->|provides| ccache__auspc
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  _policy_agenix_rekey_role_dev__0_["<policy:agenix-rekey-role-dev>[0]"]:::_policy_agenix_rekey_role_dev__0__c
  _policy_agenix_rekey_role_gaming__1_["<policy:agenix-rekey-role-gaming>[1]"]:::_policy_agenix_rekey_role_gaming__1__c
  _policy_agenix_rekey_role_gui__2_["<policy:agenix-rekey-role-gui>[2]"]:::_policy_agenix_rekey_role_gui__2__c
  _policy_agenix_rekey__to_users__3_["<policy:agenix-rekey/to-users>[3]"]:::_policy_agenix_rekey__to_users__3__c
  _policy_auscyber_role_dev__4_["<policy:auscyber-role-dev>[4]"]:::_policy_auscyber_role_dev__4__c
  _policy_auscyber_role_gaming__5_["<policy:auscyber-role-gaming>[5]"]:::_policy_auscyber_role_gaming__5__c
  _policy_auscyber_role_gui__6_["<policy:auscyber-role-gui>[6]"]:::_policy_auscyber_role_gui__6__c
  _policy_auscyber__to_hosts__8_["<policy:auscyber/to-hosts>[8]"]:::_policy_auscyber__to_hosts__8__c
  _policy_celler_push_role_dev__9_["<policy:celler-push-role-dev>[9]"]:::_policy_celler_push_role_dev__9__c
  _policy_celler_push_role_gaming__10_["<policy:celler-push-role-gaming>[10]"]:::_policy_celler_push_role_gaming__10__c
  _policy_celler_push_role_gui__11_["<policy:celler-push-role-gui>[11]"]:::_policy_celler_push_role_gui__11__c
  _policy_default_role_dev__12_["<policy:default-role-dev>[12]"]:::_policy_default_role_dev__12__c
  _policy_default_role_gaming__13_["<policy:default-role-gaming>[13]"]:::_policy_default_role_gaming__13__c
  _policy_default_role_gui__14_["<policy:default-role-gui>[14]"]:::_policy_default_role_gui__14__c
  _policy_default__to_hosts__15_["<policy:default/to-hosts>[15]"]:::_policy_default__to_hosts__15__c
  _policy_define_user_role_dev__16_["<policy:define-user-role-dev>[16]"]:::_policy_define_user_role_dev__16__c
  _policy_define_user_role_gaming__17_["<policy:define-user-role-gaming>[17]"]:::_policy_define_user_role_gaming__17__c
  _policy_define_user_role_gui__18_["<policy:define-user-role-gui>[18]"]:::_policy_define_user_role_gui__18__c
  _policy_difftastic_role_dev__19_["<policy:difftastic-role-dev>[19]"]:::_policy_difftastic_role_dev__19__c
  _policy_difftastic_role_gaming__20_["<policy:difftastic-role-gaming>[20]"]:::_policy_difftastic_role_gaming__20__c
  _policy_difftastic_role_gui__21_["<policy:difftastic-role-gui>[21]"]:::_policy_difftastic_role_gui__21__c
  _policy_extra_registry_role_dev__22_["<policy:extra-registry-role-dev>[22]"]:::_policy_extra_registry_role_dev__22__c
  _policy_extra_registry_role_gaming__23_["<policy:extra-registry-role-gaming>[23]"]:::_policy_extra_registry_role_gaming__23__c
  _policy_extra_registry_role_gui__24_["<policy:extra-registry-role-gui>[24]"]:::_policy_extra_registry_role_gui__24__c
  _policy_fish_role_dev__25_["<policy:fish-role-dev>[25]"]:::_policy_fish_role_dev__25__c
  _policy_fish_role_gaming__26_["<policy:fish-role-gaming>[26]"]:::_policy_fish_role_gaming__26__c
  _policy_fish_role_gui__27_["<policy:fish-role-gui>[27]"]:::_policy_fish_role_gui__27__c
  _policy_fonts_role_dev__29_["<policy:fonts-role-dev>[29]"]:::_policy_fonts_role_dev__29__c
  _policy_fonts_role_gaming__30_["<policy:fonts-role-gaming>[30]"]:::_policy_fonts_role_gaming__30__c
  _policy_fonts_role_gui__31_["<policy:fonts-role-gui>[31]"]:::_policy_fonts_role_gui__31__c
  _policy_hm_user_detect__32_["<policy:hm-user-detect>[32]"]:::_policy_hm_user_detect__32__c
  _policy_home_base_role_dev__34_["<policy:home-base-role-dev>[34]"]:::_policy_home_base_role_dev__34__c
  _policy_home_base_role_gaming__35_["<policy:home-base-role-gaming>[35]"]:::_policy_home_base_role_gaming__35__c
  _policy_home_base_role_gui__36_["<policy:home-base-role-gui>[36]"]:::_policy_home_base_role_gui__36__c
  _policy_hostname_role_dev__37_["<policy:hostname-role-dev>[37]"]:::_policy_hostname_role_dev__37__c
  _policy_hostname_role_gaming__38_["<policy:hostname-role-gaming>[38]"]:::_policy_hostname_role_gaming__38__c
  _policy_hostname_role_gui__39_["<policy:hostname-role-gui>[39]"]:::_policy_hostname_role_gui__39__c
  _policy_inputs__role_dev__40_["<policy:inputs'-role-dev>[40]"]:::_policy_inputs__role_dev__40__c
  _policy_inputs__role_gaming__41_["<policy:inputs'-role-gaming>[41]"]:::_policy_inputs__role_gaming__41__c
  _policy_inputs__role_gui__42_["<policy:inputs'-role-gui>[42]"]:::_policy_inputs__role_gui__42__c
  _policy_insecure_predicate_role_dev__43_["<policy:insecure-predicate-role-dev>[43]"]:::_policy_insecure_predicate_role_dev__43__c
  _policy_insecure_predicate_role_gaming__44_["<policy:insecure-predicate-role-gaming>[44]"]:::_policy_insecure_predicate_role_gaming__44__c
  _policy_insecure_predicate_role_gui__45_["<policy:insecure-predicate-role-gui>[45]"]:::_policy_insecure_predicate_role_gui__45__c
  _policy_ivy_fetch_role_dev__46_["<policy:ivy-fetch-role-dev>[46]"]:::_policy_ivy_fetch_role_dev__46__c
  _policy_ivy_fetch_role_gaming__47_["<policy:ivy-fetch-role-gaming>[47]"]:::_policy_ivy_fetch_role_gaming__47__c
  _policy_ivy_fetch_role_gui__48_["<policy:ivy-fetch-role-gui>[48]"]:::_policy_ivy_fetch_role_gui__48__c
  _policy_jujutsu_role_dev__49_["<policy:jujutsu-role-dev>[49]"]:::_policy_jujutsu_role_dev__49__c
  _policy_jujutsu_role_gaming__50_["<policy:jujutsu-role-gaming>[50]"]:::_policy_jujutsu_role_gaming__50__c
  _policy_jujutsu_role_gui__51_["<policy:jujutsu-role-gui>[51]"]:::_policy_jujutsu_role_gui__51__c
  _policy_lib_role_dev__52_["<policy:lib-role-dev>[52]"]:::_policy_lib_role_dev__52__c
  _policy_lib_role_gaming__53_["<policy:lib-role-gaming>[53]"]:::_policy_lib_role_gaming__53__c
  _policy_lib_role_gui__54_["<policy:lib-role-gui>[54]"]:::_policy_lib_role_gui__54__c
  _policy_lix_role_dev__55_["<policy:lix-role-dev>[55]"]:::_policy_lix_role_dev__55__c
  _policy_lix_role_gaming__56_["<policy:lix-role-gaming>[56]"]:::_policy_lix_role_gaming__56__c
  _policy_lix_role_gui__57_["<policy:lix-role-gui>[57]"]:::_policy_lix_role_gui__57__c
  _policy_main_ssh_key_role_dev__58_["<policy:main-ssh-key-role-dev>[58]"]:::_policy_main_ssh_key_role_dev__58__c
  _policy_main_ssh_key_role_gaming__59_["<policy:main-ssh-key-role-gaming>[59]"]:::_policy_main_ssh_key_role_gaming__59__c
  _policy_main_ssh_key_role_gui__60_["<policy:main-ssh-key-role-gui>[60]"]:::_policy_main_ssh_key_role_gui__60__c
  _policy_main_ssh_key__to_hosts__61_["<policy:main-ssh-key/to-hosts>[61]"]:::_policy_main_ssh_key__to_hosts__61__c
  _policy_nix_index_role_dev__62_["<policy:nix-index-role-dev>[62]"]:::_policy_nix_index_role_dev__62__c
  _policy_nix_index_role_gaming__63_["<policy:nix-index-role-gaming>[63]"]:::_policy_nix_index_role_gaming__63__c
  _policy_nix_index_role_gui__64_["<policy:nix-index-role-gui>[64]"]:::_policy_nix_index_role_gui__64__c
  _policy_nix_role_dev__65_["<policy:nix-role-dev>[65]"]:::_policy_nix_role_dev__65__c
  _policy_nix_role_gaming__66_["<policy:nix-role-gaming>[66]"]:::_policy_nix_role_gaming__66__c
  _policy_nix_role_gui__67_["<policy:nix-role-gui>[67]"]:::_policy_nix_role_gui__67__c
  _policy_nixpkgs_config_role_dev__68_["<policy:nixpkgs-config-role-dev>[68]"]:::_policy_nixpkgs_config_role_dev__68__c
  _policy_nixpkgs_config_role_gaming__69_["<policy:nixpkgs-config-role-gaming>[69]"]:::_policy_nixpkgs_config_role_gaming__69__c
  _policy_nixpkgs_config_role_gui__70_["<policy:nixpkgs-config-role-gui>[70]"]:::_policy_nixpkgs_config_role_gui__70__c
  _policy_nixvim_user_forward__71_["<policy:nixvim-user-forward>[71]"]:::_policy_nixvim_user_forward__71__c
  _policy_overlays_role_dev__72_["<policy:overlays-role-dev>[72]"]:::_policy_overlays_role_dev__72__c
  _policy_overlays_role_gaming__73_["<policy:overlays-role-gaming>[73]"]:::_policy_overlays_role_gaming__73__c
  _policy_overlays_role_gui__74_["<policy:overlays-role-gui>[74]"]:::_policy_overlays_role_gui__74__c
  _policy_pam_rssh_role_dev__75_["<policy:pam-rssh-role-dev>[75]"]:::_policy_pam_rssh_role_dev__75__c
  _policy_pam_rssh_role_gaming__76_["<policy:pam-rssh-role-gaming>[76]"]:::_policy_pam_rssh_role_gaming__76__c
  _policy_pam_rssh_role_gui__77_["<policy:pam-rssh-role-gui>[77]"]:::_policy_pam_rssh_role_gui__77__c
  _policy_self__role_dev__78_["<policy:self'-role-dev>[78]"]:::_policy_self__role_dev__78__c
  _policy_self__role_gaming__79_["<policy:self'-role-gaming>[79]"]:::_policy_self__role_gaming__79__c
  _policy_self__role_gui__80_["<policy:self'-role-gui>[80]"]:::_policy_self__role_gui__80__c
  _policy_shell_role_dev__81_["<policy:shell-role-dev>[81]"]:::_policy_shell_role_dev__81__c
  _policy_shell_role_gaming__82_["<policy:shell-role-gaming>[82]"]:::_policy_shell_role_gaming__82__c
  _policy_shell_role_gui__83_["<policy:shell-role-gui>[83]"]:::_policy_shell_role_gui__83__c
  _policy_sources_role_dev__85_["<policy:sources-role-dev>[85]"]:::_policy_sources_role_dev__85__c
  _policy_sources_role_gaming__86_["<policy:sources-role-gaming>[86]"]:::_policy_sources_role_gaming__86__c
  _policy_sources_role_gui__87_["<policy:sources-role-gui>[87]"]:::_policy_sources_role_gui__87__c
  _policy_starship_role_dev__88_["<policy:starship-role-dev>[88]"]:::_policy_starship_role_dev__88__c
  _policy_starship_role_gaming__89_["<policy:starship-role-gaming>[89]"]:::_policy_starship_role_gaming__89__c
  _policy_starship_role_gui__90_["<policy:starship-role-gui>[90]"]:::_policy_starship_role_gui__90__c
  _policy_unfree_predicate_role_dev__91_["<policy:unfree-predicate-role-dev>[91]"]:::_policy_unfree_predicate_role_dev__91__c
  _policy_unfree_predicate_role_gaming__92_["<policy:unfree-predicate-role-gaming>[92]"]:::_policy_unfree_predicate_role_gaming__92__c
  _policy_unfree_predicate_role_gui__93_["<policy:unfree-predicate-role-gui>[93]"]:::_policy_unfree_predicate_role_gui__93__c
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  agenix_rekey_role_dev["agenix-rekey-role-dev"]:::agenix_rekey_role_dev_c
  agenix_rekey_role_gaming["agenix-rekey-role-gaming"]:::agenix_rekey_role_gaming_c
  agenix_rekey_role_gui["agenix-rekey-role-gui"]:::agenix_rekey_role_gui_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber_role_dev["auscyber-role-dev"]:::auscyber_role_dev_c
  auscyber_role_gaming["auscyber-role-gaming"]:::auscyber_role_gaming_c
  auscyber_role_gui["auscyber-role-gui"]:::auscyber_role_gui_c
  auscyber__auspc["auscyber/auspc"]:::auscyber__auspc_c
  auscyber__to_hosts["auscyber/to-hosts"]:::auscyber__to_hosts_c
  celler_push["celler-push"]:::celler_push_c
  celler_push_role_dev["celler-push-role-dev"]:::celler_push_role_dev_c
  celler_push_role_gaming["celler-push-role-gaming"]:::celler_push_role_gaming_c
  celler_push_role_gui["celler-push-role-gui"]:::celler_push_role_gui_c
  default_user_auscyber["default"]:::default_user_auscyber_c
  default_role_dev["default-role-dev"]:::default_role_dev_c
  default_role_gaming["default-role-gaming"]:::default_role_gaming_c
  default_role_gui["default-role-gui"]:::default_role_gui_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  define_user_role_dev["define-user-role-dev"]:::define_user_role_dev_c
  define_user_role_gaming["define-user-role-gaming"]:::define_user_role_gaming_c
  define_user_role_gui["define-user-role-gui"]:::define_user_role_gui_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_nix["dev-nix"]:::dev_nix_c
  difftastic_role_dev["difftastic-role-dev"]:::difftastic_role_dev_c
  difftastic_role_gaming["difftastic-role-gaming"]:::difftastic_role_gaming_c
  difftastic_role_gui["difftastic-role-gui"]:::difftastic_role_gui_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  extra_registry_role_dev["extra-registry-role-dev"]:::extra_registry_role_dev_c
  extra_registry_role_gaming["extra-registry-role-gaming"]:::extra_registry_role_gaming_c
  extra_registry_role_gui["extra-registry-role-gui"]:::extra_registry_role_gui_c
  fish["fish"]:::fish_c
  fish_role_dev["fish-role-dev"]:::fish_role_dev_c
  fish_role_gaming["fish-role-gaming"]:::fish_role_gaming_c
  fish_role_gui["fish-role-gui"]:::fish_role_gui_c
  fish___anon__4__to_hosts["fish/<anon>:4/to-hosts"]:::fish___anon__4__to_hosts_c
  fonts_user_auscyber["fonts"]:::fonts_user_auscyber_c
  fonts_role_dev["fonts-role-dev"]:::fonts_role_dev_c
  fonts_role_gaming["fonts-role-gaming"]:::fonts_role_gaming_c
  fonts_role_gui["fonts-role-gui"]:::fonts_role_gui_c
  packages__ghostty[/"packages/ghostty"\]:::packages__ghostty_c
  ghostty["ghostty"]:::ghostty_c
  gpg["gpg"]:::gpg_c
  gui["gui"]:::gui_c
  packages__helium[/"packages/helium"\]:::packages__helium_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  home_base_role_dev["home-base-role-dev"]:::home_base_role_dev_c
  home_base_role_gaming["home-base-role-gaming"]:::home_base_role_gaming_c
  home_base_role_gui["home-base-role-gui"]:::home_base_role_gui_c
  hostname_role_dev["hostname-role-dev"]:::hostname_role_dev_c
  hostname_role_gaming["hostname-role-gaming"]:::hostname_role_gaming_c
  hostname_role_gui["hostname-role-gui"]:::hostname_role_gui_c
  inputs__role_dev["inputs'-role-dev"]:::inputs__role_dev_c
  inputs__role_gaming["inputs'-role-gaming"]:::inputs__role_gaming_c
  inputs__role_gui["inputs'-role-gui"]:::inputs__role_gui_c
  insecure_predicate_role_dev["insecure-predicate-role-dev"]:::insecure_predicate_role_dev_c
  insecure_predicate_role_gaming["insecure-predicate-role-gaming"]:::insecure_predicate_role_gaming_c
  insecure_predicate_role_gui["insecure-predicate-role-gui"]:::insecure_predicate_role_gui_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy_fetch_role_dev["ivy-fetch-role-dev"]:::ivy_fetch_role_dev_c
  ivy_fetch_role_gaming["ivy-fetch-role-gaming"]:::ivy_fetch_role_gaming_c
  ivy_fetch_role_gui["ivy-fetch-role-gui"]:::ivy_fetch_role_gui_c
  jujutsu_role_dev["jujutsu-role-dev"]:::jujutsu_role_dev_c
  jujutsu_role_gaming["jujutsu-role-gaming"]:::jujutsu_role_gaming_c
  jujutsu_role_gui["jujutsu-role-gui"]:::jujutsu_role_gui_c
  kind_system_routes_user_auscyber["kind-system-routes"]:::kind_system_routes_user_auscyber_c
  lib_role_dev["lib-role-dev"]:::lib_role_dev_c
  lib_role_gaming["lib-role-gaming"]:::lib_role_gaming_c
  lib_role_gui["lib-role-gui"]:::lib_role_gui_c
  lix["lix"]:::lix_c
  lix_role_dev["lix-role-dev"]:::lix_role_dev_c
  lix_role_gaming["lix-role-gaming"]:::lix_role_gaming_c
  lix_role_gui["lix-role-gui"]:::lix_role_gui_c
  packages__lspmux[/"packages/lspmux"\]:::packages__lspmux_c
  lspmux["lspmux"]:::lspmux_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key_role_dev["main-ssh-key-role-dev"]:::main_ssh_key_role_dev_c
  main_ssh_key_role_gaming["main-ssh-key-role-gaming"]:::main_ssh_key_role_gaming_c
  main_ssh_key_role_gui["main-ssh-key-role-gui"]:::main_ssh_key_role_gui_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nix_index_role_dev["nix-index-role-dev"]:::nix_index_role_dev_c
  nix_index_role_gaming["nix-index-role-gaming"]:::nix_index_role_gaming_c
  nix_index_role_gui["nix-index-role-gui"]:::nix_index_role_gui_c
  nix_role_dev["nix-role-dev"]:::nix_role_dev_c
  nix_role_gaming["nix-role-gaming"]:::nix_role_gaming_c
  nix_role_gui["nix-role-gui"]:::nix_role_gui_c
  nix_to_host_user_auscyber["nix-to-host"]:::nix_to_host_user_auscyber_c
  nixpkgs_config_user_auscyber["nixpkgs-config"]:::nixpkgs_config_user_auscyber_c
  nixpkgs_config_role_dev["nixpkgs-config-role-dev"]:::nixpkgs_config_role_dev_c
  nixpkgs_config_role_gaming["nixpkgs-config-role-gaming"]:::nixpkgs_config_role_gaming_c
  nixpkgs_config_role_gui["nixpkgs-config-role-gui"]:::nixpkgs_config_role_gui_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_auscyber["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_auscyber_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  nushell["nushell"]:::nushell_c
  onepassword["onepassword"]:::onepassword_c
  onepassword___when__5["onepassword/<when>:5"]:::onepassword___when__5_c
  os_to_host_user_auscyber["os-to-host"]:::os_to_host_user_auscyber_c
  overlays_role_dev["overlays-role-dev"]:::overlays_role_dev_c
  overlays_role_gaming["overlays-role-gaming"]:::overlays_role_gaming_c
  overlays_role_gui["overlays-role-gui"]:::overlays_role_gui_c
  overlays_to__overlays_user_auscyber["overlays-to-_overlays"]:::overlays_to__overlays_user_auscyber_c
  overlays_to_flake_parts_user_auscyber["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_auscyber_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  pam_rssh_role_dev["pam-rssh-role-dev"]:::pam_rssh_role_dev_c
  pam_rssh_role_gaming["pam-rssh-role-gaming"]:::pam_rssh_role_gaming_c
  pam_rssh_role_gui["pam-rssh-role-gui"]:::pam_rssh_role_gui_c
  pipe_unfree_user_auscyber["pipe-unfree"]:::pipe_unfree_user_auscyber_c
  plasma["plasma"]:::plasma_c
  den__batteries__primary_user_auscyber_auspc_{{"batteries/primary-user(auscyber@auspc)"}}:::den__batteries__primary_user_auscyber_auspc__c
  packages__proton_ge_bin[/"packages/proton-ge-bin"\]:::packages__proton_ge_bin_c
  route_casks_user_auscyber["route-casks"]:::route_casks_user_auscyber_c
  rust["rust"]:::rust_c
  self__role_dev["self'-role-dev"]:::self__role_dev_c
  self__role_gaming["self'-role-gaming"]:::self__role_gaming_c
  self__role_gui["self'-role-gui"]:::self__role_gui_c
  shell_user_auscyber["shell"]:::shell_user_auscyber_c
  shell_role_dev["shell-role-dev"]:::shell_role_dev_c
  shell_role_gaming["shell-role-gaming"]:::shell_role_gaming_c
  shell_role_gui["shell-role-gui"]:::shell_role_gui_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  sources_role_dev["sources-role-dev"]:::sources_role_dev_c
  sources_role_gaming["sources-role-gaming"]:::sources_role_gaming_c
  sources_role_gui["sources-role-gui"]:::sources_role_gui_c
  starship_role_dev["starship-role-dev"]:::starship_role_dev_c
  starship_role_gaming["starship-role-gaming"]:::starship_role_gaming_c
  starship_role_gui["starship-role-gui"]:::starship_role_gui_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_castlabs_electron__user_auscyber{{"den/provides/unfree(castlabs-electron)"}}:::den__provides__unfree_castlabs_electron__user_auscyber_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  unfree_predicate_role_dev["unfree-predicate-role-dev"]:::unfree_predicate_role_dev_c
  unfree_predicate_role_gaming["unfree-predicate-role-gaming"]:::unfree_predicate_role_gaming_c
  unfree_predicate_role_gui["unfree-predicate-role-gui"]:::unfree_predicate_role_gui_c
  user["user"]:::user_c
  user_pwd["user-pwd"]:::user_pwd_c
  user_shell__auscyber_auspc{{"user-shell/auscyber@auspc"}}:::user_shell__auscyber_auspc_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  auscyber__user__resolve_auspc__auscyber{{"auscyber/user/resolve(auspc):auscyber"}}:::auscyber__user__resolve_auspc__auscyber_c
  user__resolve_dev_nix_{{"user/resolve(dev-nix)"}}:::user__resolve_dev_nix__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_lspmux_{{"user/resolve(lspmux)"}}:::user__resolve_lspmux__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  wakatime["wakatime"]:::wakatime_c
  _policy_auscyber__to_hosts__8_ --> user_pwd
  _policy_default_role_gaming__13_ --> packages__proton_ge_bin
  auscyber --> celler_push
  auscyber --> fish
  auscyber --> lix
  auscyber --> den__provides__unfree_castlabs_electron__user_auscyber
  auscyber__auspc --> dev
  auscyber__auspc --> gpg
  auscyber__auspc --> gui
  auscyber__auspc --> packages__helium
  auscyber__auspc --> neovim
  auscyber__auspc --> nushell
  auscyber__auspc --> plasma
  auscyber__auspc --> den__batteries__primary_user_auscyber_auspc_
  auscyber__auspc --> stylix
  auscyber__auspc --> auscyber__user__resolve_auspc__auscyber
  celler_push --> agenix_rekey_user_auscyber
  dev --> dev_cli
  dev --> dev_nix
  dev_nix --> user__resolve_dev_nix_
  fish --> shell_user_auscyber
  fish --> user_shell__auscyber_auspc
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ghostty --> packages__ghostty
  gui --> ghostty
  gui --> onepassword
  ivy_fetch --> packages__ivy_fetch
  lspmux --> packages__lspmux
  lspmux --> user__resolve__anon__4_
  lspmux --> user__resolve_lspmux_
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  nixvim --> packages__eagle_nvim
  nixvim --> rust
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  onepassword --> onepassword___when__5
  onepassword --> den__provides__unfree_onepassword_password_manager_
  rust --> lspmux
  user --> _policy_agenix_rekey_role_dev__0_
  user --> _policy_agenix_rekey_role_gaming__1_
  user --> _policy_agenix_rekey_role_gui__2_
  user --> _policy_agenix_rekey__to_users__3_
  user --> _policy_auscyber_role_dev__4_
  user --> _policy_auscyber_role_gaming__5_
  user --> _policy_auscyber_role_gui__6_
  user --> _policy_auscyber__to_hosts__8_
  user --> _policy_celler_push_role_dev__9_
  user --> _policy_celler_push_role_gaming__10_
  user --> _policy_celler_push_role_gui__11_
  user --> _policy_default_role_dev__12_
  user --> _policy_default_role_gaming__13_
  user --> _policy_default_role_gui__14_
  user --> _policy_default__to_hosts__15_
  user --> _policy_define_user_role_dev__16_
  user --> _policy_define_user_role_gaming__17_
  user --> _policy_define_user_role_gui__18_
  user --> _policy_difftastic_role_dev__19_
  user --> _policy_difftastic_role_gaming__20_
  user --> _policy_difftastic_role_gui__21_
  user --> _policy_extra_registry_role_dev__22_
  user --> _policy_extra_registry_role_gaming__23_
  user --> _policy_extra_registry_role_gui__24_
  user --> _policy_fish_role_dev__25_
  user --> _policy_fish_role_gaming__26_
  user --> _policy_fish_role_gui__27_
  user --> _policy_fonts_role_dev__29_
  user --> _policy_fonts_role_gaming__30_
  user --> _policy_fonts_role_gui__31_
  user --> _policy_hm_user_detect__32_
  user --> _policy_home_base_role_dev__34_
  user --> _policy_home_base_role_gaming__35_
  user --> _policy_home_base_role_gui__36_
  user --> _policy_hostname_role_dev__37_
  user --> _policy_hostname_role_gaming__38_
  user --> _policy_hostname_role_gui__39_
  user --> _policy_inputs__role_dev__40_
  user --> _policy_inputs__role_gaming__41_
  user --> _policy_inputs__role_gui__42_
  user --> _policy_insecure_predicate_role_dev__43_
  user --> _policy_insecure_predicate_role_gaming__44_
  user --> _policy_insecure_predicate_role_gui__45_
  user --> _policy_ivy_fetch_role_dev__46_
  user --> _policy_ivy_fetch_role_gaming__47_
  user --> _policy_ivy_fetch_role_gui__48_
  user --> _policy_jujutsu_role_dev__49_
  user --> _policy_jujutsu_role_gaming__50_
  user --> _policy_jujutsu_role_gui__51_
  user --> _policy_lib_role_dev__52_
  user --> _policy_lib_role_gaming__53_
  user --> _policy_lib_role_gui__54_
  user --> _policy_lix_role_dev__55_
  user --> _policy_lix_role_gaming__56_
  user --> _policy_lix_role_gui__57_
  user --> _policy_main_ssh_key_role_dev__58_
  user --> _policy_main_ssh_key_role_gaming__59_
  user --> _policy_main_ssh_key_role_gui__60_
  user --> _policy_main_ssh_key__to_hosts__61_
  user --> _policy_nix_index_role_dev__62_
  user --> _policy_nix_index_role_gaming__63_
  user --> _policy_nix_index_role_gui__64_
  user --> _policy_nix_role_dev__65_
  user --> _policy_nix_role_gaming__66_
  user --> _policy_nix_role_gui__67_
  user --> _policy_nixpkgs_config_role_dev__68_
  user --> _policy_nixpkgs_config_role_gaming__69_
  user --> _policy_nixpkgs_config_role_gui__70_
  user --> _policy_nixvim_user_forward__71_
  user --> _policy_overlays_role_dev__72_
  user --> _policy_overlays_role_gaming__73_
  user --> _policy_overlays_role_gui__74_
  user --> _policy_pam_rssh_role_dev__75_
  user --> _policy_pam_rssh_role_gaming__76_
  user --> _policy_pam_rssh_role_gui__77_
  user --> _policy_self__role_dev__78_
  user --> _policy_self__role_gaming__79_
  user --> _policy_self__role_gui__80_
  user --> _policy_shell_role_dev__81_
  user --> _policy_shell_role_gaming__82_
  user --> _policy_shell_role_gui__83_
  user --> _policy_sources_role_dev__85_
  user --> _policy_sources_role_gaming__86_
  user --> _policy_sources_role_gui__87_
  user --> _policy_starship_role_dev__88_
  user --> _policy_starship_role_gaming__89_
  user --> _policy_starship_role_gui__90_
  user --> _policy_unfree_predicate_role_dev__91_
  user --> _policy_unfree_predicate_role_gaming__92_
  user --> _policy_unfree_predicate_role_gui__93_
  user --> auscyber
  user --> auscyber__auspc
  user --> default_user_auscyber
  user --> fonts_user_auscyber
  user --> ivy_fetch
  user --> main_ssh_key
  user --> nixpkgs_config_user_auscyber
  user --> shell__to_users
  user --> user__resolve_user_
  auscyber -.->|provides| auscyber__user__resolve_auspc__auscyber
  end

  auscyber -.->|provides| auscyber__auspc__to_users
  neovim -.->|provides| neovim__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey_role_dev__0__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey_role_gaming__1__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey_role_gui__2__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey__to_users__3__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_alx_wol_role_dev__0__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_alx_wol_role_gaming__1__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_alx_wol_role_gui__2__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber_role_dev__4__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber_role_gaming__5__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber_role_gui__6__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber__auspc___anon__6__to_hosts__3__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-width:2px
  classDef _policy_auscyber__to_hosts__8__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auspc_role_dev__4__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auspc_role_gaming__5__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auspc_role_gui__6__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_bootlogo_role_dev__8__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_bootlogo_role_gaming__9__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_bootlogo_role_gui__10__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_builder_server_role_dev__11__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_builder_server_role_gaming__12__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_builder_server_role_gui__13__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_cachyos_kernel_role_dev__14__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_cachyos_kernel_role_gaming__15__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_cachyos_kernel_role_gui__16__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ccache_role_dev__17__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ccache_role_gaming__18__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ccache_role_gui__19__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_dev__9__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_gaming__10__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_gui__11__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_dev__21__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_gaming__22__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_gui__23__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_dev__24__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_gaming__25__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_gui__26__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_dev__27__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_gaming__28__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_gui__29__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_dev__30__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_gaming__31__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_gui__32__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default_role_dev__12__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default_role_gaming__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef _policy_default_role_gui__14__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__15__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_dev__16__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_gaming__17__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_gui__18__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_cli_role_dev__33__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_cli_role_gaming__34__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_cli_role_gui__35__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_nix_role_dev__36__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_nix_role_gaming__37__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_nix_role_gui__38__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_role_dev__39__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_role_gaming__40__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_role_gui__41__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_dev__19__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_gaming__20__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_gui__21__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_disko_role_dev__42__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_disko_role_gaming__43__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_disko_role_gui__44__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_dev__45__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_gaming__46__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_gui__47__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_dev__22__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_gaming__23__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_gui__24__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_facter_role_dev__48__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_facter_role_gaming__49__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_facter_role_gui__50__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_dev__25__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_gaming__26__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_gui__27__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_dev__29__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_gaming__30__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_gui__31__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ghostty_role_dev__51__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ghostty_role_gaming__52__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ghostty_role_gui__53__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_dev__54__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_gaming__55__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_gui__56__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpus_role_dev__57__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpus_role_gaming__58__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpus_role_gui__59__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gui_role_dev__60__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gui_role_gaming__61__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gui_role_gui__62__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-width:2px
  classDef _policy_hm_user_detect__32__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_dev__34__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_gaming__35__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_gui__36__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_dev__37__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_gaming__38__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_gui__39__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_dev__40__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_gaming__41__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_gui__42__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_dev__43__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_gaming__44__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_gui__45__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_dev__46__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_gaming__47__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_gui__48__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_dev__49__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_gaming__50__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_gui__51__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_dev__52__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_gaming__53__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_gui__54__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lix_role_dev__55__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lix_role_gaming__56__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lix_role_gui__57__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_dev__63__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_gaming__64__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_gui__65__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_dev__58__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_gaming__59__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_gui__60__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__61__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef _policy_neovim_role_dev__66__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_neovim_role_gaming__67__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_neovim_role_gui__68__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_dev__62__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_gaming__63__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_gui__64__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_dev__65__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_gaming__66__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_gui__67__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_dev__70__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_gaming__71__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_gui__72__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_dev__68__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_gaming__69__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_gui__70__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_dev__73__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_gaming__74__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_gui__75__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_user_forward__71__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nushell_role_dev__76__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nushell_role_gaming__77__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nushell_role_gui__78__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_onepassword_role_dev__79__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_onepassword_role_gaming__80__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_onepassword_role_gui__81__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_dev__82__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_gaming__83__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_gui__84__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_dev__72__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_gaming__73__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_gui__74__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_dev__75__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_gaming__76__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_gui__77__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_dev__85__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_gaming__86__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_gui__87__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_plasma_role_dev__88__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_plasma_role_gaming__89__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_plasma_role_gui__90__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_dev__91__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_gaming__92__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_gui__93__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_secure_boot_role_dev__94__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_secure_boot_role_gaming__95__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_secure_boot_role_gui__96__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_dev__78__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_gaming__79__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_gui__80__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_dev__81__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_gaming__82__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_gui__83__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_dev__85__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_gaming__86__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_gui__87__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_dev__88__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_gaming__89__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_gui__90__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_dev__97__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_gaming__98__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_gui__99__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_dev__100__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_gaming__101__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_gui__102__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_dev__91__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_gaming__92__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_gui__93__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_user_pwd_role_dev__103__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_user_pwd_role_gaming__104__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_user_pwd_role_gui__105__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_role_dev__106__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_role_gaming__107__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_role_gui__108__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_secrets_role_dev__109__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_secrets_role_gaming__110__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_secrets_role_gui__111__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config_role_dev__112__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config_role_gaming__113__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config_role_gui__114__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config__to_users__115__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_dev__116__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_gaming__117__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_gui__118__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey_role_gaming_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__alx_wol_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber_role_gaming_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber__auspc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef ccache__auspc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef bootlogo_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef builder_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef cachyos_kernel_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef ccache_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef celler_push_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef celler_push_role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef celler_push_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default_role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef define_user_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef define_user_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef define_user_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user__auscyber_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef difftastic_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef difftastic_role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef difftastic_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef disko_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef extra_registry_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef extra_registry_role_gaming_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef extra_registry_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef facter_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fish_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish_role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish___anon__4__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_role_gaming_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef packages__ghostty_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpus_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef packages__helium_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef home_base_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_role_gaming_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_gpus__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_vpn__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef hostname_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef hostname_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef hostname_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef inputs__role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef inputs__role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef inputs__role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate_role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivy_fetch_role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivy_fetch_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_host_auspc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_auscyber_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lix_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef lix_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lix_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lix_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key_role_gaming_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_index_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_index_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_index_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nix___when__5_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixpkgs_config_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixpkgs_config_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef onepassword___when__5_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_rssh_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_rssh_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_rssh_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef plasma_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_auscyber_auspc__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef packages__proton_ge_bin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef route_casks_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef secure_boot_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef self__role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef self__role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef self__role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef sources_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sources_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sources_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef starship_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef starship_role_gaming_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef starship_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef auscyber__auspc__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_castlabs_electron__host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_castlabs_electron__user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate_role_gaming_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_pwd_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef user_shell__auscyber_auspc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef auscyber__user__resolve_auspc__auscyber_c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_dev_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_lspmux__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef vpn_ssh_config_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_auspc fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_auscyber fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## contabo

**Architecture:** `x86_64-linux`

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  contabo([contabo]):::root

  subgraph ctx_host_contabo["host: contabo"]
  _anon_["<anon>"]:::_anon__c
  _policy_fish___anon__4__to_hosts__0_["<policy:fish/<anon>:4/to-hosts>[0]"]:::_policy_fish___anon__4__to_hosts__0__c
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_contabo["default"]:::default_host_contabo_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivy_contabo{{"batteries/define-user/ivy@contabo"}}:::den__batteries__define_user__ivy_contabo_c
  difftastic["difftastic"]:::difftastic_c
  extra_registry["extra-registry"]:::extra_registry_c
  fonts_host_contabo["fonts"]:::fonts_host_contabo_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_contabo["kind-system-routes"]:::kind_system_routes_host_contabo_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_contabo["nix-to-host"]:::nix_to_host_host_contabo_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_contabo["nixpkgs-config"]:::nixpkgs_config_host_contabo_c
  nixvim_include_global_pkgs_host_contabo["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_contabo_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_contabo["os-to-host"]:::os_to_host_host_contabo_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_contabo["overlays-to-_overlays"]:::overlays_to__overlays_host_contabo_c
  overlays_to_flake_parts_host_contabo["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_contabo_c
  pam_rssh_host_contabo["pam-rssh"]:::pam_rssh_host_contabo_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_contabo["pipe-unfree"]:::pipe_unfree_host_contabo_c
  route_casks_host_contabo["route-casks"]:::route_casks_host_contabo_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell["shell"]:::shell_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_contabo --> agenix_rekey
  default_host_contabo --> den__batteries__define_user
  default_host_contabo --> extra_registry
  default_host_contabo --> home_base
  default_host_contabo --> host__resolve_default_
  default_host_contabo --> den__batteries__hostname
  default_host_contabo --> den__batteries__inputs_
  default_host_contabo --> insecure_predicate
  default_host_contabo --> lib
  default_host_contabo --> nix
  default_host_contabo --> overlays
  default_host_contabo --> den__batteries__self_
  default_host_contabo --> den__batteries__sources
  default_host_contabo --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__ivy_contabo
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_contabo --> fonts___when__4
  fonts_host_contabo --> fonts___when__5
  home_base --> shell
  host --> contabo
  host --> darwin_base
  host --> default_host_contabo
  host --> fonts_host_contabo
  host --> host__resolve_host_
  host --> nixos_general
  host --> nixpkgs_config_host_contabo
  host --> openssh
  host --> pam_rssh_host_contabo
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  nix --> nix___when__4
  nixpkgs_config_host_contabo --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell --> jujutsu
  shell --> nix_index
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivy["user: ivy"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_default__to_hosts__1_["<policy:default/to-hosts>[1]"]:::_policy_default__to_hosts__1__c
  _policy_hm_user_detect__2_["<policy:hm-user-detect>[2]"]:::_policy_hm_user_detect__2__c
  _policy_main_ssh_key__to_hosts__5_["<policy:main-ssh-key/to-hosts>[5]"]:::_policy_main_ssh_key__to_hosts__5__c
  _policy_nixvim_user_forward__6_["<policy:nixvim-user-forward>[6]"]:::_policy_nixvim_user_forward__6__c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_ivy["default"]:::default_user_ivy_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  fish["fish"]:::fish_c
  fonts_user_ivy["fonts"]:::fonts_user_ivy_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  ivy{{"ivy"}}:::ivy_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy__contabo["ivy/contabo"]:::ivy__contabo_c
  kind_system_routes_user_ivy["kind-system-routes"]:::kind_system_routes_user_ivy_c
  packages__lspmux[/"packages/lspmux"\]:::packages__lspmux_c
  lspmux["lspmux"]:::lspmux_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nix_to_host_user_ivy["nix-to-host"]:::nix_to_host_user_ivy_c
  nixpkgs_config_user_ivy["nixpkgs-config"]:::nixpkgs_config_user_ivy_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_ivy["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_ivy_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  os_to_host_user_ivy["os-to-host"]:::os_to_host_user_ivy_c
  overlays_to__overlays_user_ivy["overlays-to-_overlays"]:::overlays_to__overlays_user_ivy_c
  overlays_to_flake_parts_user_ivy["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_ivy_c
  pam_rssh_user_ivy["pam-rssh"]:::pam_rssh_user_ivy_c
  pipe_unfree_user_ivy["pipe-unfree"]:::pipe_unfree_user_ivy_c
  den__batteries__primary_user_ivy_contabo_{{"batteries/primary-user(ivy@contabo)"}}:::den__batteries__primary_user_ivy_contabo__c
  route_casks_user_ivy["route-casks"]:::route_casks_user_ivy_c
  rust["rust"]:::rust_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  user["user"]:::user_c
  user_shell__ivy_contabo{{"user-shell/ivy@contabo"}}:::user_shell__ivy_contabo_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_lspmux_{{"user/resolve(lspmux)"}}:::user__resolve_lspmux__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  wakatime["wakatime"]:::wakatime_c
  fish --> user_shell__ivy_contabo
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  ivy__contabo --> fish
  ivy__contabo --> neovim
  ivy__contabo --> den__batteries__primary_user_ivy_contabo_
  lspmux --> packages__lspmux
  lspmux --> user__resolve__anon__4_
  lspmux --> user__resolve_lspmux_
  main_ssh_key --> pam_rssh_user_ivy
  neovim --> nixvim
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> rust
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  rust --> lspmux
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_default__to_hosts__1_
  user --> _policy_hm_user_detect__2_
  user --> _policy_main_ssh_key__to_hosts__5_
  user --> _policy_nixvim_user_forward__6_
  user --> ivy__contabo
  user --> default_user_ivy
  user --> fonts_user_ivy
  user --> ivy
  user --> ivy_fetch
  user --> main_ssh_key
  user --> nixpkgs_config_user_ivy
  user --> shell__to_users
  user --> user__resolve_user_
  end

  neovim -.->|provides| neovim__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish___anon__4__to_hosts__0__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__5__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef _policy_nixvim_user_forward__6__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivy_contabo_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy__contabo_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_contabo_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_ivy_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_to_host_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_contabo_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_contabo_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_to__overlays_host_contabo_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_contabo_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivy_contabo__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef route_casks_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivy_contabo_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_lspmux__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_contabo fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_ivy fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## imflopet

**Architecture:** `x86_64-linux`

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  imflopet([imflopet]):::root

  subgraph ctx_host_imflopet["host: imflopet"]
  _anon_["<anon>"]:::_anon__c
  _policy_fish___anon__4__to_hosts__0_["<policy:fish/<anon>:4/to-hosts>[0]"]:::_policy_fish___anon__4__to_hosts__0__c
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_imflopet["default"]:::default_host_imflopet_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivy_imflopet{{"batteries/define-user/ivy@imflopet"}}:::den__batteries__define_user__ivy_imflopet_c
  difftastic["difftastic"]:::difftastic_c
  extra_registry["extra-registry"]:::extra_registry_c
  fonts_host_imflopet["fonts"]:::fonts_host_imflopet_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_imflopet["kind-system-routes"]:::kind_system_routes_host_imflopet_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_imflopet["nix-to-host"]:::nix_to_host_host_imflopet_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_imflopet["nixpkgs-config"]:::nixpkgs_config_host_imflopet_c
  nixvim_include_global_pkgs_host_imflopet["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_imflopet_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_imflopet["os-to-host"]:::os_to_host_host_imflopet_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_imflopet["overlays-to-_overlays"]:::overlays_to__overlays_host_imflopet_c
  overlays_to_flake_parts_host_imflopet["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_imflopet_c
  pam_rssh_host_imflopet["pam-rssh"]:::pam_rssh_host_imflopet_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_imflopet["pipe-unfree"]:::pipe_unfree_host_imflopet_c
  route_casks_host_imflopet["route-casks"]:::route_casks_host_imflopet_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell["shell"]:::shell_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  ivy__imflopet__to_users[/"imflopet/to-users"\]:::ivy__imflopet__to_users_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_imflopet --> agenix_rekey
  default_host_imflopet --> den__batteries__define_user
  default_host_imflopet --> extra_registry
  default_host_imflopet --> home_base
  default_host_imflopet --> host__resolve_default_
  default_host_imflopet --> den__batteries__hostname
  default_host_imflopet --> den__batteries__inputs_
  default_host_imflopet --> insecure_predicate
  default_host_imflopet --> lib
  default_host_imflopet --> nix
  default_host_imflopet --> overlays
  default_host_imflopet --> den__batteries__self_
  default_host_imflopet --> den__batteries__sources
  default_host_imflopet --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__ivy_imflopet
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_imflopet --> fonts___when__4
  fonts_host_imflopet --> fonts___when__5
  home_base --> shell
  host --> darwin_base
  host --> default_host_imflopet
  host --> fonts_host_imflopet
  host --> host__resolve_host_
  host --> imflopet
  host --> nixos_general
  host --> nixpkgs_config_host_imflopet
  host --> openssh
  host --> pam_rssh_host_imflopet
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  nix --> nix___when__4
  nixpkgs_config_host_imflopet --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell --> jujutsu
  shell --> nix_index
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivy["user: ivy"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_default__to_hosts__1_["<policy:default/to-hosts>[1]"]:::_policy_default__to_hosts__1__c
  _policy_hm_user_detect__2_["<policy:hm-user-detect>[2]"]:::_policy_hm_user_detect__2__c
  _policy_main_ssh_key__to_hosts__5_["<policy:main-ssh-key/to-hosts>[5]"]:::_policy_main_ssh_key__to_hosts__5__c
  _policy_nixvim_user_forward__6_["<policy:nixvim-user-forward>[6]"]:::_policy_nixvim_user_forward__6__c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_ivy["default"]:::default_user_ivy_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  fish["fish"]:::fish_c
  fonts_user_ivy["fonts"]:::fonts_user_ivy_c
  gpg["gpg"]:::gpg_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  ivy{{"ivy"}}:::ivy_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy__imflopet["ivy/imflopet"]:::ivy__imflopet_c
  kind_system_routes_user_ivy["kind-system-routes"]:::kind_system_routes_user_ivy_c
  packages__lspmux[/"packages/lspmux"\]:::packages__lspmux_c
  lspmux["lspmux"]:::lspmux_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nix_to_host_user_ivy["nix-to-host"]:::nix_to_host_user_ivy_c
  nixpkgs_config_user_ivy["nixpkgs-config"]:::nixpkgs_config_user_ivy_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_ivy["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_ivy_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  os_to_host_user_ivy["os-to-host"]:::os_to_host_user_ivy_c
  overlays_to__overlays_user_ivy["overlays-to-_overlays"]:::overlays_to__overlays_user_ivy_c
  overlays_to_flake_parts_user_ivy["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_ivy_c
  pam_rssh_user_ivy["pam-rssh"]:::pam_rssh_user_ivy_c
  pipe_unfree_user_ivy["pipe-unfree"]:::pipe_unfree_user_ivy_c
  den__batteries__primary_user_ivy_imflopet_{{"batteries/primary-user(ivy@imflopet)"}}:::den__batteries__primary_user_ivy_imflopet__c
  route_casks_user_ivy["route-casks"]:::route_casks_user_ivy_c
  rust["rust"]:::rust_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  user["user"]:::user_c
  user_shell__ivy_imflopet{{"user-shell/ivy@imflopet"}}:::user_shell__ivy_imflopet_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_lspmux_{{"user/resolve(lspmux)"}}:::user__resolve_lspmux__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  wakatime["wakatime"]:::wakatime_c
  fish --> user_shell__ivy_imflopet
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  ivy__imflopet --> fish
  ivy__imflopet --> gpg
  ivy__imflopet --> neovim
  ivy__imflopet --> den__batteries__primary_user_ivy_imflopet_
  lspmux --> packages__lspmux
  lspmux --> user__resolve__anon__4_
  lspmux --> user__resolve_lspmux_
  main_ssh_key --> pam_rssh_user_ivy
  neovim --> nixvim
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> rust
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  rust --> lspmux
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_default__to_hosts__1_
  user --> _policy_hm_user_detect__2_
  user --> _policy_main_ssh_key__to_hosts__5_
  user --> _policy_nixvim_user_forward__6_
  user --> default_user_ivy
  user --> fonts_user_ivy
  user --> ivy__imflopet
  user --> ivy
  user --> ivy_fetch
  user --> main_ssh_key
  user --> nixpkgs_config_user_ivy
  user --> shell__to_users
  user --> user__resolve_user_
  end

  ivy -.->|provides| ivy__imflopet__to_users
  neovim -.->|provides| neovim__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish___anon__4__to_hosts__0__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__5__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef _policy_nixvim_user_forward__6__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivy_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy__imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_imflopet_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_ivy_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_to_host_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_to__overlays_host_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivy_imflopet__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef route_casks_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy__imflopet__to_users_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivy_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_lspmux__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_imflopet fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_ivy fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## lora-pi

**Architecture:** `aarch64-linux`

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  lora_pi([lora-pi]):::root

  subgraph ctx_host_lora_pi["host: lora-pi"]
  _anon_["<anon>"]:::_anon__c
  _policy_fish___anon__4__to_hosts__0_["<policy:fish/<anon>:4/to-hosts>[0]"]:::_policy_fish___anon__4__to_hosts__0__c
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_lora_pi["default"]:::default_host_lora_pi_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivy_lora_pi{{"batteries/define-user/ivy@lora-pi"}}:::den__batteries__define_user__ivy_lora_pi_c
  difftastic["difftastic"]:::difftastic_c
  extra_registry["extra-registry"]:::extra_registry_c
  fonts_host_lora_pi["fonts"]:::fonts_host_lora_pi_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_lora_pi["kind-system-routes"]:::kind_system_routes_host_lora_pi_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_lora_pi["nix-to-host"]:::nix_to_host_host_lora_pi_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixos_raspberrypi["nixos-raspberrypi"]:::nixos_raspberrypi_c
  nixpkgs_config_host_lora_pi["nixpkgs-config"]:::nixpkgs_config_host_lora_pi_c
  nixvim_include_global_pkgs_host_lora_pi["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_lora_pi_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_lora_pi["os-to-host"]:::os_to_host_host_lora_pi_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_lora_pi["overlays-to-_overlays"]:::overlays_to__overlays_host_lora_pi_c
  overlays_to_flake_parts_host_lora_pi["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_lora_pi_c
  pam_rssh_host_lora_pi["pam-rssh"]:::pam_rssh_host_lora_pi_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_lora_pi["pipe-unfree"]:::pipe_unfree_host_lora_pi_c
  route_casks_host_lora_pi["route-casks"]:::route_casks_host_lora_pi_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell["shell"]:::shell_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_lora_pi --> agenix_rekey
  default_host_lora_pi --> den__batteries__define_user
  default_host_lora_pi --> extra_registry
  default_host_lora_pi --> home_base
  default_host_lora_pi --> host__resolve_default_
  default_host_lora_pi --> den__batteries__hostname
  default_host_lora_pi --> den__batteries__inputs_
  default_host_lora_pi --> insecure_predicate
  default_host_lora_pi --> lib
  default_host_lora_pi --> nix
  default_host_lora_pi --> overlays
  default_host_lora_pi --> den__batteries__self_
  default_host_lora_pi --> den__batteries__sources
  default_host_lora_pi --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__ivy_lora_pi
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_lora_pi --> fonts___when__4
  fonts_host_lora_pi --> fonts___when__5
  home_base --> shell
  host --> darwin_base
  host --> default_host_lora_pi
  host --> fonts_host_lora_pi
  host --> host__resolve_host_
  host --> lora_pi
  host --> nixos_general
  host --> nixpkgs_config_host_lora_pi
  host --> openssh
  host --> pam_rssh_host_lora_pi
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  lora_pi --> nixos_raspberrypi
  nix --> nix___when__4
  nixpkgs_config_host_lora_pi --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell --> jujutsu
  shell --> nix_index
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivy["user: ivy"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_default__to_hosts__1_["<policy:default/to-hosts>[1]"]:::_policy_default__to_hosts__1__c
  _policy_hm_user_detect__2_["<policy:hm-user-detect>[2]"]:::_policy_hm_user_detect__2__c
  _policy_main_ssh_key__to_hosts__5_["<policy:main-ssh-key/to-hosts>[5]"]:::_policy_main_ssh_key__to_hosts__5__c
  _policy_nixvim_user_forward__6_["<policy:nixvim-user-forward>[6]"]:::_policy_nixvim_user_forward__6__c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_ivy["default"]:::default_user_ivy_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  fish["fish"]:::fish_c
  fonts_user_ivy["fonts"]:::fonts_user_ivy_c
  gpg["gpg"]:::gpg_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  ivy{{"ivy"}}:::ivy_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy__lora_pi["ivy/lora-pi"]:::ivy__lora_pi_c
  kind_system_routes_user_ivy["kind-system-routes"]:::kind_system_routes_user_ivy_c
  packages__lspmux[/"packages/lspmux"\]:::packages__lspmux_c
  lspmux["lspmux"]:::lspmux_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nix_to_host_user_ivy["nix-to-host"]:::nix_to_host_user_ivy_c
  nixpkgs_config_user_ivy["nixpkgs-config"]:::nixpkgs_config_user_ivy_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_ivy["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_ivy_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  os_to_host_user_ivy["os-to-host"]:::os_to_host_user_ivy_c
  overlays_to__overlays_user_ivy["overlays-to-_overlays"]:::overlays_to__overlays_user_ivy_c
  overlays_to_flake_parts_user_ivy["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_ivy_c
  pam_rssh_user_ivy["pam-rssh"]:::pam_rssh_user_ivy_c
  pipe_unfree_user_ivy["pipe-unfree"]:::pipe_unfree_user_ivy_c
  den__batteries__primary_user_ivy_lora_pi_{{"batteries/primary-user(ivy@lora-pi)"}}:::den__batteries__primary_user_ivy_lora_pi__c
  route_casks_user_ivy["route-casks"]:::route_casks_user_ivy_c
  rust["rust"]:::rust_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  user["user"]:::user_c
  user_shell__ivy_lora_pi{{"user-shell/ivy@lora-pi"}}:::user_shell__ivy_lora_pi_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_lspmux_{{"user/resolve(lspmux)"}}:::user__resolve_lspmux__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  wakatime["wakatime"]:::wakatime_c
  fish --> user_shell__ivy_lora_pi
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  ivy__lora_pi --> fish
  ivy__lora_pi --> gpg
  ivy__lora_pi --> neovim
  ivy__lora_pi --> den__batteries__primary_user_ivy_lora_pi_
  lspmux --> packages__lspmux
  lspmux --> user__resolve__anon__4_
  lspmux --> user__resolve_lspmux_
  main_ssh_key --> pam_rssh_user_ivy
  neovim --> nixvim
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> rust
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  rust --> lspmux
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_default__to_hosts__1_
  user --> _policy_hm_user_detect__2_
  user --> _policy_main_ssh_key__to_hosts__5_
  user --> _policy_nixvim_user_forward__6_
  user --> default_user_ivy
  user --> fonts_user_ivy
  user --> ivy
  user --> ivy_fetch
  user --> ivy__lora_pi
  user --> main_ssh_key
  user --> nixpkgs_config_user_ivy
  user --> shell__to_users
  user --> user__resolve_user_
  end

  neovim -.->|provides| neovim__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish___anon__4__to_hosts__0__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__5__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef _policy_nixvim_user_forward__6__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivy_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy__lora_pi_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_lora_pi_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_ivy_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages__lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_to_host_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos_raspberrypi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_lora_pi_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_lora_pi_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_to__overlays_host_lora_pi_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_lora_pi_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivy_lora_pi__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef route_casks_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivy_lora_pi_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_lspmux__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_lora_pi fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_ivy fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## macmini

**Architecture:** `aarch64-darwin`
**Roles:** gui

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  macmini([macmini]):::root

  subgraph ctx_host_macmini["host: macmini"]
  _anon_["<anon>"]:::_anon__c
  _policy__policy_onepassword_role_gui__51___to_hosts__0_["<policy:<policy:onepassword-role-gui>[51]/to-hosts>[0]"]:::_policy__policy_onepassword_role_gui__51___to_hosts__0__c
  _policy__policy_onepassword_role_gui__51___to_users__1_["<policy:<policy:onepassword-role-gui>[51]/to-users>[1]"]:::_policy__policy_onepassword_role_gui__51___to_users__1__c
  _policy_darwin_base_role_gui__2_["<policy:darwin-base-role-gui>[2]"]:::_policy_darwin_base_role_gui__2__c
  _policy_darwin_finder_role_gui__3_["<policy:darwin-finder-role-gui>[3]"]:::_policy_darwin_finder_role_gui__3__c
  _policy_darwin_general_role_gui__4_["<policy:darwin-general-role-gui>[4]"]:::_policy_darwin_general_role_gui__4__c
  _policy_darwin_hmApps_role_gui__5_["<policy:darwin-hmApps-role-gui>[5]"]:::_policy_darwin_hmApps_role_gui__5__c
  _policy_macmini_role_gui__6_["<policy:macmini-role-gui>[6]"]:::_policy_macmini_role_gui__6__c
  _policy_nixos_general_role_gui__7_["<policy:nixos-general-role-gui>[7]"]:::_policy_nixos_general_role_gui__7__c
  _policy_openssh_role_gui__8_["<policy:openssh-role-gui>[8]"]:::_policy_openssh_role_gui__8__c
  _policy_pam_touchid_role_gui__9_["<policy:pam-touchid-role-gui>[9]"]:::_policy_pam_touchid_role_gui__9__c
  _policy_sudoagents_role_gui__10_["<policy:sudoagents-role-gui>[10]"]:::_policy_sudoagents_role_gui__10__c
  _policy_to_users_role_gui__11_["<policy:to-users-role-gui>[11]"]:::_policy_to_users_role_gui__11__c
  agenix_rekey_host_macmini["agenix-rekey"]:::agenix_rekey_host_macmini_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_macmini["default"]:::default_host_macmini_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivypierlot_macmini{{"batteries/define-user/ivypierlot@macmini"}}:::den__batteries__define_user__ivypierlot_macmini_c
  difftastic["difftastic"]:::difftastic_c
  extra_registry["extra-registry"]:::extra_registry_c
  fonts_host_macmini["fonts"]:::fonts_host_macmini_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  homebrew_host_macmini["homebrew"]:::homebrew_host_macmini_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_macmini["kind-system-routes"]:::kind_system_routes_host_macmini_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_macmini["nix-to-host"]:::nix_to_host_host_macmini_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_macmini["nixpkgs-config"]:::nixpkgs_config_host_macmini_c
  nixvim_include_global_pkgs_host_macmini["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_macmini_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_macmini["os-to-host"]:::os_to_host_host_macmini_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_macmini["overlays-to-_overlays"]:::overlays_to__overlays_host_macmini_c
  overlays_to_flake_parts_host_macmini["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_macmini_c
  pam_rssh_host_macmini["pam-rssh"]:::pam_rssh_host_macmini_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_macmini["pipe-unfree"]:::pipe_unfree_host_macmini_c
  route_casks_host_macmini["route-casks"]:::route_casks_host_macmini_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_macmini["shell"]:::shell_host_macmini_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_macmini --> agenix_rekey_host_macmini
  default_host_macmini --> den__batteries__define_user
  default_host_macmini --> extra_registry
  default_host_macmini --> home_base
  default_host_macmini --> host__resolve_default_
  default_host_macmini --> den__batteries__hostname
  default_host_macmini --> den__batteries__inputs_
  default_host_macmini --> insecure_predicate
  default_host_macmini --> lib
  default_host_macmini --> nix
  default_host_macmini --> overlays
  default_host_macmini --> den__batteries__self_
  default_host_macmini --> den__batteries__sources
  default_host_macmini --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__ivypierlot_macmini
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_macmini --> fonts___when__4
  fonts_host_macmini --> fonts___when__5
  home_base --> shell_host_macmini
  host --> darwin_base
  host --> default_host_macmini
  host --> fonts_host_macmini
  host --> host__resolve_host_
  host --> macmini
  host --> nixos_general
  host --> nixpkgs_config_host_macmini
  host --> openssh
  host --> pam_rssh_host_macmini
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  macmini --> homebrew_host_macmini
  nix --> nix___when__4
  nixpkgs_config_host_macmini --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_macmini --> jujutsu
  shell_host_macmini --> nix_index
  shell_host_macmini --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivypierlot["user: ivypierlot"]
  _policy_agenix_rekey_role_gui__0_["<policy:agenix-rekey-role-gui>[0]"]:::_policy_agenix_rekey_role_gui__0__c
  _policy_agenix_rekey__to_users__1_["<policy:agenix-rekey/to-users>[1]"]:::_policy_agenix_rekey__to_users__1__c
  _policy_celler_push_role_gui__3_["<policy:celler-push-role-gui>[3]"]:::_policy_celler_push_role_gui__3__c
  _policy_cotabby_role_gui__4_["<policy:cotabby-role-gui>[4]"]:::_policy_cotabby_role_gui__4__c
  _policy_default_role_gui__5_["<policy:default-role-gui>[5]"]:::_policy_default_role_gui__5__c
  _policy_default__to_hosts__6_["<policy:default/to-hosts>[6]"]:::_policy_default__to_hosts__6__c
  _policy_define_user_role_gui__7_["<policy:define-user-role-gui>[7]"]:::_policy_define_user_role_gui__7__c
  _policy_dev_cli_role_gui__8_["<policy:dev-cli-role-gui>[8]"]:::_policy_dev_cli_role_gui__8__c
  _policy_dev_nix_role_gui__9_["<policy:dev-nix-role-gui>[9]"]:::_policy_dev_nix_role_gui__9__c
  _policy_dev_role_gui__10_["<policy:dev-role-gui>[10]"]:::_policy_dev_role_gui__10__c
  _policy_difftastic_role_gui__11_["<policy:difftastic-role-gui>[11]"]:::_policy_difftastic_role_gui__11__c
  _policy_eagle_nvim_role_gui__12_["<policy:eagle-nvim-role-gui>[12]"]:::_policy_eagle_nvim_role_gui__12__c
  _policy_extra_registry_role_gui__13_["<policy:extra-registry-role-gui>[13]"]:::_policy_extra_registry_role_gui__13__c
  _policy_file_local_role_gui__14_["<policy:file-local-role-gui>[14]"]:::_policy_file_local_role_gui__14__c
  _policy_fish_role_gui__15_["<policy:fish-role-gui>[15]"]:::_policy_fish_role_gui__15__c
  _policy_fonts_role_gui__17_["<policy:fonts-role-gui>[17]"]:::_policy_fonts_role_gui__17__c
  _policy_ghostty_role_gui__18_["<policy:ghostty-role-gui>[18]"]:::_policy_ghostty_role_gui__18__c
  _policy_gpg_role_gui__19_["<policy:gpg-role-gui>[19]"]:::_policy_gpg_role_gui__19__c
  _policy_gui_role_gui__20_["<policy:gui-role-gui>[20]"]:::_policy_gui_role_gui__20__c
  _policy_hm_user_detect__21_["<policy:hm-user-detect>[21]"]:::_policy_hm_user_detect__21__c
  _policy_home_base_role_gui__23_["<policy:home-base-role-gui>[23]"]:::_policy_home_base_role_gui__23__c
  _policy_homebrew_role_gui__24_["<policy:homebrew-role-gui>[24]"]:::_policy_homebrew_role_gui__24__c
  _policy_hostname_role_gui__25_["<policy:hostname-role-gui>[25]"]:::_policy_hostname_role_gui__25__c
  _policy_idris_role_gui__26_["<policy:idris-role-gui>[26]"]:::_policy_idris_role_gui__26__c
  _policy_inputs__role_gui__27_["<policy:inputs'-role-gui>[27]"]:::_policy_inputs__role_gui__27__c
  _policy_insecure_predicate_role_gui__28_["<policy:insecure-predicate-role-gui>[28]"]:::_policy_insecure_predicate_role_gui__28__c
  _policy_ivy_fetch_role_gui__29_["<policy:ivy-fetch-role-gui>[29]"]:::_policy_ivy_fetch_role_gui__29__c
  _policy_ivypierlot_role_gui__30_["<policy:ivypierlot-role-gui>[30]"]:::_policy_ivypierlot_role_gui__30__c
  _policy_jankyborders_role_gui__32_["<policy:jankyborders-role-gui>[32]"]:::_policy_jankyborders_role_gui__32__c
  _policy_jj_mcp_server_role_gui__33_["<policy:jj-mcp-server-role-gui>[33]"]:::_policy_jj_mcp_server_role_gui__33__c
  _policy_jujutsu_role_gui__34_["<policy:jujutsu-role-gui>[34]"]:::_policy_jujutsu_role_gui__34__c
  _policy_kanata_role_gui__35_["<policy:kanata-role-gui>[35]"]:::_policy_kanata_role_gui__35__c
  _policy_lib_role_gui__37_["<policy:lib-role-gui>[37]"]:::_policy_lib_role_gui__37__c
  _policy_llama_cpp_role_gui__38_["<policy:llama-cpp-role-gui>[38]"]:::_policy_llama_cpp_role_gui__38__c
  _policy_lspmux_role_gui__39_["<policy:lspmux-role-gui>[39]"]:::_policy_lspmux_role_gui__39__c
  _policy_main_ssh_key_role_gui__40_["<policy:main-ssh-key-role-gui>[40]"]:::_policy_main_ssh_key_role_gui__40__c
  _policy_main_ssh_key__to_hosts__41_["<policy:main-ssh-key/to-hosts>[41]"]:::_policy_main_ssh_key__to_hosts__41__c
  _policy_mcp_servers_role_gui__42_["<policy:mcp-servers-role-gui>[42]"]:::_policy_mcp_servers_role_gui__42__c
  _policy_neovim_role_gui__43_["<policy:neovim-role-gui>[43]"]:::_policy_neovim_role_gui__43__c
  _policy_nix_index_role_gui__45_["<policy:nix-index-role-gui>[45]"]:::_policy_nix_index_role_gui__45__c
  _policy_nix_role_gui__46_["<policy:nix-role-gui>[46]"]:::_policy_nix_role_gui__46__c
  _policy_nixpkgs_config_role_gui__47_["<policy:nixpkgs-config-role-gui>[47]"]:::_policy_nixpkgs_config_role_gui__47__c
  _policy_nixvim_role_gui__48_["<policy:nixvim-role-gui>[48]"]:::_policy_nixvim_role_gui__48__c
  _policy_nixvim_user_forward__49_["<policy:nixvim-user-forward>[49]"]:::_policy_nixvim_user_forward__49__c
  _policy_nushell_role_gui__50_["<policy:nushell-role-gui>[50]"]:::_policy_nushell_role_gui__50__c
  _policy_onepassword_role_gui__51_["<policy:onepassword-role-gui>[51]"]:::_policy_onepassword_role_gui__51__c
  _policy_openclaw_role_gui__52_["<policy:openclaw-role-gui>[52]"]:::_policy_openclaw_role_gui__52__c
  _policy_opencode_role_gui__53_["<policy:opencode-role-gui>[53]"]:::_policy_opencode_role_gui__53__c
  _policy_overlays_role_gui__54_["<policy:overlays-role-gui>[54]"]:::_policy_overlays_role_gui__54__c
  _policy_pam_rssh_role_gui__55_["<policy:pam-rssh-role-gui>[55]"]:::_policy_pam_rssh_role_gui__55__c
  _policy_rift_role_gui__56_["<policy:rift-role-gui>[56]"]:::_policy_rift_role_gui__56__c
  _policy_rust_role_gui__57_["<policy:rust-role-gui>[57]"]:::_policy_rust_role_gui__57__c
  _policy_self__role_gui__58_["<policy:self'-role-gui>[58]"]:::_policy_self__role_gui__58__c
  _policy_shell_role_gui__59_["<policy:shell-role-gui>[59]"]:::_policy_shell_role_gui__59__c
  _policy_sketchybar_role_gui__61_["<policy:sketchybar-role-gui>[61]"]:::_policy_sketchybar_role_gui__61__c
  _policy_sketchybar_app_font_role_gui__62_["<policy:sketchybar_app_font-role-gui>[62]"]:::_policy_sketchybar_app_font_role_gui__62__c
  _policy_sources_role_gui__63_["<policy:sources-role-gui>[63]"]:::_policy_sources_role_gui__63__c
  _policy_starship_role_gui__64_["<policy:starship-role-gui>[64]"]:::_policy_starship_role_gui__64__c
  _policy_stylix_role_gui__65_["<policy:stylix-role-gui>[65]"]:::_policy_stylix_role_gui__65__c
  _policy_unfree_predicate_role_gui__66_["<policy:unfree-predicate-role-gui>[66]"]:::_policy_unfree_predicate_role_gui__66__c
  _policy_wakatime_role_gui__67_["<policy:wakatime-role-gui>[67]"]:::_policy_wakatime_role_gui__67__c
  _policy_zen_role_gui__68_["<policy:zen-role-gui>[68]"]:::_policy_zen_role_gui__68__c
  _policy_zotero_mcp_role_gui__69_["<policy:zotero-mcp-role-gui>[69]"]:::_policy_zotero_mcp_role_gui__69__c
  agenix_rekey_user_ivypierlot["agenix-rekey"]:::agenix_rekey_user_ivypierlot_c
  agenix_rekey_role_gui["agenix-rekey-role-gui"]:::agenix_rekey_role_gui_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  browsers__zen___when__4___anon__0__to_hosts["browsers/zen/<when>:4/<anon>:0/to-hosts"]:::browsers__zen___when__4___anon__0__to_hosts_c
  celler_push["celler-push"]:::celler_push_c
  celler_push_role_gui["celler-push-role-gui"]:::celler_push_role_gui_c
  packages__cotabby[/"packages/cotabby"\]:::packages__cotabby_c
  cotabby["cotabby"]:::cotabby_c
  cotabby_role_gui["cotabby-role-gui"]:::cotabby_role_gui_c
  default_user_ivypierlot["default"]:::default_user_ivypierlot_c
  default_role_gui["default-role-gui"]:::default_role_gui_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  define_user_role_gui["define-user-role-gui"]:::define_user_role_gui_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_cli_role_gui["dev-cli-role-gui"]:::dev_cli_role_gui_c
  dev_nix["dev-nix"]:::dev_nix_c
  dev_nix_role_gui["dev-nix-role-gui"]:::dev_nix_role_gui_c
  dev_role_gui["dev-role-gui"]:::dev_role_gui_c
  difftastic_role_gui["difftastic-role-gui"]:::difftastic_role_gui_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  eagle_nvim_role_gui["eagle-nvim-role-gui"]:::eagle_nvim_role_gui_c
  extra_registry_role_gui["extra-registry-role-gui"]:::extra_registry_role_gui_c
  file_local["file-local"]:::file_local_c
  file_local_role_gui["file-local-role-gui"]:::file_local_role_gui_c
  fish["fish"]:::fish_c
  fish_role_gui["fish-role-gui"]:::fish_role_gui_c
  fish___anon__4__to_hosts["fish/<anon>:4/to-hosts"]:::fish___anon__4__to_hosts_c
  fonts_user_ivypierlot["fonts"]:::fonts_user_ivypierlot_c
  fonts_role_gui["fonts-role-gui"]:::fonts_role_gui_c
  packages__ghostty[/"packages/ghostty"\]:::packages__ghostty_c
  ghostty["ghostty"]:::ghostty_c
  ghostty_role_gui["ghostty-role-gui"]:::ghostty_role_gui_c
  gpg["gpg"]:::gpg_c
  gpg_role_gui["gpg-role-gui"]:::gpg_role_gui_c
  gui["gui"]:::gui_c
  gui_role_gui["gui-role-gui"]:::gui_role_gui_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  home_base_role_gui["home-base-role-gui"]:::home_base_role_gui_c
  homebrew_user_ivypierlot["homebrew"]:::homebrew_user_ivypierlot_c
  homebrew_role_gui["homebrew-role-gui"]:::homebrew_role_gui_c
  hostname_role_gui["hostname-role-gui"]:::hostname_role_gui_c
  idris["idris"]:::idris_c
  idris_role_gui["idris-role-gui"]:::idris_role_gui_c
  inputs__role_gui["inputs'-role-gui"]:::inputs__role_gui_c
  insecure_predicate_role_gui["insecure-predicate-role-gui"]:::insecure_predicate_role_gui_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy_fetch_role_gui["ivy-fetch-role-gui"]:::ivy_fetch_role_gui_c
  ivypierlot{{"ivypierlot"}}:::ivypierlot_c
  ivypierlot_role_gui["ivypierlot-role-gui"]:::ivypierlot_role_gui_c
  ivypierlot__macmini["ivypierlot/macmini"]:::ivypierlot__macmini_c
  packages__jankyborders[/"packages/jankyborders"\]:::packages__jankyborders_c
  jankyborders["jankyborders"]:::jankyborders_c
  jankyborders_role_gui["jankyborders-role-gui"]:::jankyborders_role_gui_c
  packages__jj_mcp_server[/"packages/jj-mcp-server"\]:::packages__jj_mcp_server_c
  jj_mcp_server_role_gui["jj-mcp-server-role-gui"]:::jj_mcp_server_role_gui_c
  jujutsu_role_gui["jujutsu-role-gui"]:::jujutsu_role_gui_c
  kanata["kanata"]:::kanata_c
  kanata_role_gui["kanata-role-gui"]:::kanata_role_gui_c
  packages__kanata_tray[/"packages/kanata-tray"\]:::packages__kanata_tray_c
  kanata___anon__4__to_hosts["kanata/<anon>:4/to-hosts"]:::kanata___anon__4__to_hosts_c
  kind_system_routes_user_ivypierlot["kind-system-routes"]:::kind_system_routes_user_ivypierlot_c
  lib_role_gui["lib-role-gui"]:::lib_role_gui_c
  llama_cpp["llama-cpp"]:::llama_cpp_c
  llama_cpp_role_gui["llama-cpp-role-gui"]:::llama_cpp_role_gui_c
  lsp_servers_to_homeManager["lsp-servers-to-homeManager"]:::lsp_servers_to_homeManager_c
  lsp_servers_to_nvim["lsp-servers-to-nvim"]:::lsp_servers_to_nvim_c
  packages__lspmux[/"packages/lspmux"\]:::packages__lspmux_c
  lspmux["lspmux"]:::lspmux_c
  lspmux_role_gui["lspmux-role-gui"]:::lspmux_role_gui_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key_role_gui["main-ssh-key-role-gui"]:::main_ssh_key_role_gui_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  mcp_servers["mcp-servers"]:::mcp_servers_c
  mcp_servers_role_gui["mcp-servers-role-gui"]:::mcp_servers_role_gui_c
  neovim["neovim"]:::neovim_c
  neovim_role_gui["neovim-role-gui"]:::neovim_role_gui_c
  neovim__to_users["neovim/to-users"]:::neovim__to_users_c
  nh_env["nh-env"]:::nh_env_c
  nix_index_role_gui["nix-index-role-gui"]:::nix_index_role_gui_c
  nix_role_gui["nix-role-gui"]:::nix_role_gui_c
  nix_to_host_user_ivypierlot["nix-to-host"]:::nix_to_host_user_ivypierlot_c
  nixpkgs_config_user_ivypierlot["nixpkgs-config"]:::nixpkgs_config_user_ivypierlot_c
  nixpkgs_config_role_gui["nixpkgs-config-role-gui"]:::nixpkgs_config_role_gui_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_ivypierlot["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_ivypierlot_c
  nixvim_role_gui["nixvim-role-gui"]:::nixvim_role_gui_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  nushell["nushell"]:::nushell_c
  nushell_role_gui["nushell-role-gui"]:::nushell_role_gui_c
  onepassword["onepassword"]:::onepassword_c
  onepassword_role_gui["onepassword-role-gui"]:::onepassword_role_gui_c
  onepassword___when__5["onepassword/<when>:5"]:::onepassword___when__5_c
  openclaw["openclaw"]:::openclaw_c
  openclaw_role_gui["openclaw-role-gui"]:::openclaw_role_gui_c
  opencode["opencode"]:::opencode_c
  opencode_role_gui["opencode-role-gui"]:::opencode_role_gui_c
  os_to_host_user_ivypierlot["os-to-host"]:::os_to_host_user_ivypierlot_c
  overlays_role_gui["overlays-role-gui"]:::overlays_role_gui_c
  overlays_to__overlays_user_ivypierlot["overlays-to-_overlays"]:::overlays_to__overlays_user_ivypierlot_c
  overlays_to_flake_parts_user_ivypierlot["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_ivypierlot_c
  pam_rssh_user_ivypierlot["pam-rssh"]:::pam_rssh_user_ivypierlot_c
  pam_rssh_role_gui["pam-rssh-role-gui"]:::pam_rssh_role_gui_c
  pipe_unfree_user_ivypierlot["pipe-unfree"]:::pipe_unfree_user_ivypierlot_c
  den__batteries__primary_user_ivypierlot_macmini_{{"batteries/primary-user(ivypierlot@macmini)"}}:::den__batteries__primary_user_ivypierlot_macmini__c
  rift["rift"]:::rift_c
  rift_role_gui["rift-role-gui"]:::rift_role_gui_c
  route_casks_user_ivypierlot["route-casks"]:::route_casks_user_ivypierlot_c
  rust["rust"]:::rust_c
  rust_role_gui["rust-role-gui"]:::rust_role_gui_c
  self__role_gui["self'-role-gui"]:::self__role_gui_c
  shell_user_ivypierlot["shell"]:::shell_user_ivypierlot_c
  shell_role_gui["shell-role-gui"]:::shell_role_gui_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  packages__sketchybar[/"packages/sketchybar"\]:::packages__sketchybar_c
  sketchybar["sketchybar"]:::sketchybar_c
  sketchybar_role_gui["sketchybar-role-gui"]:::sketchybar_role_gui_c
  packages__sketchybar_app_font[/"packages/sketchybar_app_font"\]:::packages__sketchybar_app_font_c
  sketchybar_app_font_role_gui["sketchybar_app_font-role-gui"]:::sketchybar_app_font_role_gui_c
  sources_role_gui["sources-role-gui"]:::sources_role_gui_c
  starship_role_gui["starship-role-gui"]:::starship_role_gui_c
  stylix["stylix"]:::stylix_c
  stylix_role_gui["stylix-role-gui"]:::stylix_role_gui_c
  sudoagents["sudoagents"]:::sudoagents_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  den__provides__unfree_libkey_nomad_onepassword_password_manager_{{"provides/unfree(libkey-nomad,onepassword-password-manager)"}}:::den__provides__unfree_libkey_nomad_onepassword_password_manager__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  unfree_predicate_role_gui["unfree-predicate-role-gui"]:::unfree_predicate_role_gui_c
  user["user"]:::user_c
  user_shell__ivypierlot_macmini{{"user-shell/ivypierlot@macmini"}}:::user_shell__ivypierlot_macmini_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  user__resolve__when__4_["user/resolve(<when>:4)"]:::user__resolve__when__4__c
  user__resolve__when__5_["user/resolve(<when>:5)"]:::user__resolve__when__5__c
  user__resolve_dev_nix_{{"user/resolve(dev-nix)"}}:::user__resolve_dev_nix__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_jankyborders_{{"user/resolve(jankyborders)"}}:::user__resolve_jankyborders__c
  user__resolve_kanata_{{"user/resolve(kanata)"}}:::user__resolve_kanata__c
  user__resolve_lspmux_{{"user/resolve(lspmux)"}}:::user__resolve_lspmux__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  user__resolve_zen_{{"user/resolve(zen)"}}:::user__resolve_zen__c
  wakatime["wakatime"]:::wakatime_c
  wakatime_role_gui["wakatime-role-gui"]:::wakatime_role_gui_c
  browsers__zen[/"browsers/zen"\]:::browsers__zen_c
  zen_role_gui["zen-role-gui"]:::zen_role_gui_c
  packages__zotero_mcp[/"packages/zotero-mcp"\]:::packages__zotero_mcp_c
  zotero_mcp_role_gui["zotero-mcp-role-gui"]:::zotero_mcp_role_gui_c
  browsers__zen --> den__provides__unfree_libkey_nomad_onepassword_password_manager_
  browsers__zen --> user__resolve_zen_
  cotabby --> packages__cotabby
  dev --> dev_cli
  dev --> dev_nix
  dev_nix --> user__resolve_dev_nix_
  fish --> shell_user_ivypierlot
  fish --> user_shell__ivypierlot_macmini
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ghostty --> packages__ghostty
  idris --> lspmux
  ivy_fetch --> packages__ivy_fetch
  ivypierlot --> agenix_rekey_user_ivypierlot
  ivypierlot --> celler_push
  ivypierlot --> cotabby
  ivypierlot --> dev
  ivypierlot --> file_local
  ivypierlot --> fish
  ivypierlot --> ghostty
  ivypierlot --> gpg
  ivypierlot --> gui
  ivypierlot --> homebrew_user_ivypierlot
  ivypierlot --> idris
  ivypierlot --> kanata
  ivypierlot --> llama_cpp
  ivypierlot --> neovim
  ivypierlot --> nixvim
  ivypierlot --> nushell
  ivypierlot --> onepassword
  ivypierlot --> openclaw
  ivypierlot --> opencode
  ivypierlot --> den__batteries__primary_user_ivypierlot_macmini_
  ivypierlot --> rift
  ivypierlot --> sketchybar
  ivypierlot --> browsers__zen
  jankyborders --> packages__jankyborders
  jankyborders --> user__resolve_jankyborders_
  kanata --> packages__kanata_tray
  kanata --> user__resolve_kanata_
  lspmux --> packages__lspmux
  lspmux --> user__resolve__anon__4_
  lspmux --> user__resolve_lspmux_
  main_ssh_key --> pam_rssh_user_ivypierlot
  mcp_servers --> packages__jj_mcp_server
  mcp_servers --> packages__zotero_mcp
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> rust
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  onepassword --> onepassword___when__5
  onepassword --> den__provides__unfree_onepassword_password_manager_
  onepassword___when__5 --> user__resolve__when__5_
  opencode --> mcp_servers
  rift --> jankyborders
  sketchybar --> packages__sketchybar
  sketchybar --> packages__sketchybar_app_font
  user --> _policy_agenix_rekey_role_gui__0_
  user --> _policy_agenix_rekey__to_users__1_
  user --> _policy_celler_push_role_gui__3_
  user --> _policy_cotabby_role_gui__4_
  user --> _policy_default_role_gui__5_
  user --> _policy_default__to_hosts__6_
  user --> _policy_define_user_role_gui__7_
  user --> _policy_dev_cli_role_gui__8_
  user --> _policy_dev_nix_role_gui__9_
  user --> _policy_dev_role_gui__10_
  user --> _policy_difftastic_role_gui__11_
  user --> _policy_eagle_nvim_role_gui__12_
  user --> _policy_extra_registry_role_gui__13_
  user --> _policy_file_local_role_gui__14_
  user --> _policy_fish_role_gui__15_
  user --> _policy_fonts_role_gui__17_
  user --> _policy_ghostty_role_gui__18_
  user --> _policy_gpg_role_gui__19_
  user --> _policy_gui_role_gui__20_
  user --> _policy_hm_user_detect__21_
  user --> _policy_home_base_role_gui__23_
  user --> _policy_homebrew_role_gui__24_
  user --> _policy_hostname_role_gui__25_
  user --> _policy_idris_role_gui__26_
  user --> _policy_inputs__role_gui__27_
  user --> _policy_insecure_predicate_role_gui__28_
  user --> _policy_ivy_fetch_role_gui__29_
  user --> _policy_ivypierlot_role_gui__30_
  user --> _policy_jankyborders_role_gui__32_
  user --> _policy_jj_mcp_server_role_gui__33_
  user --> _policy_jujutsu_role_gui__34_
  user --> _policy_kanata_role_gui__35_
  user --> _policy_lib_role_gui__37_
  user --> _policy_llama_cpp_role_gui__38_
  user --> _policy_lspmux_role_gui__39_
  user --> _policy_main_ssh_key_role_gui__40_
  user --> _policy_main_ssh_key__to_hosts__41_
  user --> _policy_mcp_servers_role_gui__42_
  user --> _policy_neovim_role_gui__43_
  user --> _policy_nix_index_role_gui__45_
  user --> _policy_nix_role_gui__46_
  user --> _policy_nixpkgs_config_role_gui__47_
  user --> _policy_nixvim_role_gui__48_
  user --> _policy_nixvim_user_forward__49_
  user --> _policy_nushell_role_gui__50_
  user --> _policy_onepassword_role_gui__51_
  user --> _policy_openclaw_role_gui__52_
  user --> _policy_opencode_role_gui__53_
  user --> _policy_overlays_role_gui__54_
  user --> _policy_pam_rssh_role_gui__55_
  user --> _policy_rift_role_gui__56_
  user --> _policy_rust_role_gui__57_
  user --> _policy_self__role_gui__58_
  user --> _policy_shell_role_gui__59_
  user --> _policy_sketchybar_role_gui__61_
  user --> _policy_sketchybar_app_font_role_gui__62_
  user --> _policy_sources_role_gui__63_
  user --> _policy_starship_role_gui__64_
  user --> _policy_stylix_role_gui__65_
  user --> _policy_unfree_predicate_role_gui__66_
  user --> _policy_wakatime_role_gui__67_
  user --> _policy_zen_role_gui__68_
  user --> _policy_zotero_mcp_role_gui__69_
  user --> default_user_ivypierlot
  user --> fonts_user_ivypierlot
  user --> ivy_fetch
  user --> ivypierlot
  user --> ivypierlot__macmini
  user --> main_ssh_key
  user --> nixpkgs_config_user_ivypierlot
  user --> sudoagents
  user --> neovim__to_users
  user --> shell__to_users
  user --> user__resolve_user_
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy__policy_onepassword_role_gui__51___to_hosts__0__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy__policy_onepassword_role_gui__51___to_users__1__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey_role_gui__0__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey__to_users__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_gui__3__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_cotabby_role_gui__4__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_gui__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_gui__3__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_gui__4__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_gui__5__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default_role_gui__5__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__6__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_gui__7__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_cli_role_gui__8__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_nix_role_gui__9__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_dev_role_gui__10__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_gui__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_gui__12__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_gui__13__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_file_local_role_gui__14__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_gui__15__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_gui__17__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ghostty_role_gui__18__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_gui__19__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gui_role_gui__20__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__21__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_gui__23__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_homebrew_role_gui__24__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_gui__25__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_idris_role_gui__26__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_gui__27__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_gui__28__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_gui__29__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivypierlot_role_gui__30__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jankyborders_role_gui__32__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jj_mcp_server_role_gui__33__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_gui__34__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_kanata_role_gui__35__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_gui__37__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_llama_cpp_role_gui__38__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_gui__39__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_macmini_role_gui__6__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_gui__40__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__41__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_mcp_servers_role_gui__42__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_neovim_role_gui__43__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_gui__45__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_gui__46__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_gui__7__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_gui__47__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_gui__48__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_user_forward__49__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nushell_role_gui__50__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_onepassword_role_gui__51__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openclaw_role_gui__52__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_opencode_role_gui__53__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_gui__8__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_gui__54__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_gui__55__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_gui__9__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rift_role_gui__56__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_gui__57__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_gui__58__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_gui__59__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sketchybar_role_gui__61__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sketchybar_app_font_role_gui__62__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_gui__63__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_gui__64__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_gui__65__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sudoagents_role_gui__10__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_gui__11__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_gui__66__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_gui__67__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zen_role_gui__68__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_zotero_mcp_role_gui__69__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef browsers_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef browsers__zen___when__4___anon__0__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef celler_push_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__cotabby_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef cotabby_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef cotabby_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef define_user_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user__ivypierlot_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef dev_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef difftastic_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef eagle_nvim_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef extra_registry_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef file_local_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef file_local_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fish_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish___anon__4__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef packages__ghostty_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ghostty_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gui_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef home_base_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef homebrew_host_macmini_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef homebrew_user_ivypierlot_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef homebrew_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef hostname_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef idris_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef idris_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef inputs__role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivypierlot_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivypierlot__macmini_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__jankyborders_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef jankyborders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jankyborders_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__jj_mcp_server_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef jj_mcp_server_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kanata_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef kanata_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__kanata_tray_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef kanata___anon__4__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_host_macmini_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_ivypierlot_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef llama_cpp_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef llama_cpp_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lsp_servers_to_homeManager_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lsp_servers_to_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lspmux_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef mcp_servers_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef mcp_servers_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim__to_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_index_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nushell_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef onepassword_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef onepassword___when__5_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef openclaw_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef openclaw_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef opencode_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef opencode_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_rssh_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivypierlot_macmini__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef rift_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef rift_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef rust_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef self__role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__sketchybar_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef sketchybar_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sketchybar_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__sketchybar_app_font_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sketchybar_app_font_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef sources_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef starship_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef stylix_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sudoagents_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_libkey_nomad_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivypierlot_macmini_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve__when__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve__when__5__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_dev_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_jankyborders__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_kanata__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_lspmux__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_zen__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef wakatime_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef browsers__zen_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef zen_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__zotero_mcp_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef zotero_mcp_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
style ctx_host_macmini fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_ivypierlot fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## pentestvm

**Architecture:** `x86_64-linux`

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  pentestvm([pentestvm]):::root

  subgraph ctx_host_pentestvm["host: pentestvm"]
  _anon_["<anon>"]:::_anon__c
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_pentestvm["default"]:::default_host_pentestvm_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__admin_pentestvm{{"batteries/define-user/admin@pentestvm"}}:::den__batteries__define_user__admin_pentestvm_c
  difftastic["difftastic"]:::difftastic_c
  extra_registry["extra-registry"]:::extra_registry_c
  fonts_host_pentestvm["fonts"]:::fonts_host_pentestvm_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_pentestvm["kind-system-routes"]:::kind_system_routes_host_pentestvm_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_pentestvm["nix-to-host"]:::nix_to_host_host_pentestvm_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_pentestvm["nixpkgs-config"]:::nixpkgs_config_host_pentestvm_c
  nixvim_include_global_pkgs_host_pentestvm["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_pentestvm_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_pentestvm["os-to-host"]:::os_to_host_host_pentestvm_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_pentestvm["overlays-to-_overlays"]:::overlays_to__overlays_host_pentestvm_c
  overlays_to_flake_parts_host_pentestvm["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_pentestvm_c
  pam_rssh_host_pentestvm["pam-rssh"]:::pam_rssh_host_pentestvm_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_pentestvm["pipe-unfree"]:::pipe_unfree_host_pentestvm_c
  route_casks_host_pentestvm["route-casks"]:::route_casks_host_pentestvm_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell["shell"]:::shell_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_pentestvm --> agenix_rekey
  default_host_pentestvm --> den__batteries__define_user
  default_host_pentestvm --> extra_registry
  default_host_pentestvm --> home_base
  default_host_pentestvm --> host__resolve_default_
  default_host_pentestvm --> den__batteries__hostname
  default_host_pentestvm --> den__batteries__inputs_
  default_host_pentestvm --> insecure_predicate
  default_host_pentestvm --> lib
  default_host_pentestvm --> nix
  default_host_pentestvm --> overlays
  default_host_pentestvm --> den__batteries__self_
  default_host_pentestvm --> den__batteries__sources
  default_host_pentestvm --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__admin_pentestvm
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_pentestvm --> fonts___when__4
  fonts_host_pentestvm --> fonts___when__5
  home_base --> shell
  host --> darwin_base
  host --> default_host_pentestvm
  host --> fonts_host_pentestvm
  host --> host__resolve_host_
  host --> nixos_general
  host --> nixpkgs_config_host_pentestvm
  host --> openssh
  host --> pam_rssh_host_pentestvm
  host --> pentestvm
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  nix --> nix___when__4
  nixpkgs_config_host_pentestvm --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell --> jujutsu
  shell --> nix_index
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_admin["user: admin"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_default__to_hosts__1_["<policy:default/to-hosts>[1]"]:::_policy_default__to_hosts__1__c
  _policy_hm_user_detect__2_["<policy:hm-user-detect>[2]"]:::_policy_hm_user_detect__2__c
  _policy_main_ssh_key__to_hosts__4_["<policy:main-ssh-key/to-hosts>[4]"]:::_policy_main_ssh_key__to_hosts__4__c
  _policy_nixvim_user_forward__5_["<policy:nixvim-user-forward>[5]"]:::_policy_nixvim_user_forward__5__c
  admin{{"admin"}}:::admin_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_admin["default"]:::default_user_admin_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  fonts_user_admin["fonts"]:::fonts_user_admin_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  kind_system_routes_user_admin["kind-system-routes"]:::kind_system_routes_user_admin_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  nh_env["nh-env"]:::nh_env_c
  nix_to_host_user_admin["nix-to-host"]:::nix_to_host_user_admin_c
  nixpkgs_config_user_admin["nixpkgs-config"]:::nixpkgs_config_user_admin_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_admin["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_admin_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  os_to_host_user_admin["os-to-host"]:::os_to_host_user_admin_c
  overlays_to__overlays_user_admin["overlays-to-_overlays"]:::overlays_to__overlays_user_admin_c
  overlays_to_flake_parts_user_admin["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_admin_c
  pam_rssh_user_admin["pam-rssh"]:::pam_rssh_user_admin_c
  pipe_unfree_user_admin["pipe-unfree"]:::pipe_unfree_user_admin_c
  den__batteries__primary_user_admin_pentestvm_{{"batteries/primary-user(admin@pentestvm)"}}:::den__batteries__primary_user_admin_pentestvm__c
  route_casks_user_admin["route-casks"]:::route_casks_user_admin_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  user["user"]:::user_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  admin --> den__batteries__primary_user_admin_pentestvm_
  ivy_fetch --> packages__ivy_fetch
  main_ssh_key --> pam_rssh_user_admin
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_default__to_hosts__1_
  user --> _policy_hm_user_detect__2_
  user --> _policy_main_ssh_key__to_hosts__4_
  user --> _policy_nixvim_user_forward__5_
  user --> admin
  user --> default_user_admin
  user --> fonts_user_admin
  user --> ivy_fetch
  user --> main_ssh_key
  user --> nixpkgs_config_user_admin
  user --> shell__to_users
  user --> user__resolve_user_
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__4__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef _policy_nixvim_user_forward__5__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef admin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__admin_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef fonts_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_pentestvm_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_admin_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_to_host_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_pentestvm_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_admin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_pentestvm_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_admin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_to__overlays_host_pentestvm_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_admin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_pentestvm_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_admin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pentestvm_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_admin_pentestvm__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef route_casks_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
style ctx_host_pentestvm fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_admin fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## secondpc

**Architecture:** `x86_64-linux`

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  secondpc([secondpc]):::root

  subgraph ctx_host_secondpc["host: secondpc"]
  _anon_["<anon>"]:::_anon__c
  _policy_vpn_ssh_config__to_users__1_["<policy:vpn-ssh-config/to-users>[1]"]:::_policy_vpn_ssh_config__to_users__1__c
  agenix_rekey_host_secondpc["agenix-rekey"]:::agenix_rekey_host_secondpc_c
  builder_server["builder-server"]:::builder_server_c
  builders["builders"]:::builders_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_secondpc["default"]:::default_host_secondpc_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__auscyber_secondpc{{"batteries/define-user/auscyber@secondpc"}}:::den__batteries__define_user__auscyber_secondpc_c
  difftastic["difftastic"]:::difftastic_c
  disko["disko"]:::disko_c
  extra_registry["extra-registry"]:::extra_registry_c
  facter["facter"]:::facter_c
  fonts_host_secondpc["fonts"]:::fonts_host_secondpc_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__4_["host/resolve(<when>:4)"]:::host__resolve__when__4__c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nginx_{{"host/resolve(nginx)"}}:::host__resolve_nginx__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  host__resolve_vpn_{{"host/resolve(vpn)"}}:::host__resolve_vpn__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_secondpc["kind-system-routes"]:::kind_system_routes_host_secondpc_c
  lib["lib"]:::lib_c
  local["local"]:::local_c
  nginx_host_secondpc["nginx"]:::nginx_host_secondpc_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_secondpc["nix-to-host"]:::nix_to_host_host_secondpc_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nix___when__5["nix/<when>:5"]:::nix___when__5_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_secondpc["nixpkgs-config"]:::nixpkgs_config_host_secondpc_c
  nixvim_include_global_pkgs_host_secondpc["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_secondpc_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_secondpc["os-to-host"]:::os_to_host_host_secondpc_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_secondpc["overlays-to-_overlays"]:::overlays_to__overlays_host_secondpc_c
  overlays_to_flake_parts_host_secondpc["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_secondpc_c
  pam_rssh_host_secondpc["pam-rssh"]:::pam_rssh_host_secondpc_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_secondpc["pipe-unfree"]:::pipe_unfree_host_secondpc_c
  route_casks_host_secondpc["route-casks"]:::route_casks_host_secondpc_c
  searchix["searchix"]:::searchix_c
  secondpc_web["secondpc-web"]:::secondpc_web_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_secondpc["shell"]:::shell_host_secondpc_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  den__provides__unfree_intel_ocl_{{"provides/unfree(intel-ocl)"}}:::den__provides__unfree_intel_ocl__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  user_pwd_host_secondpc["user-pwd"]:::user_pwd_host_secondpc_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  vpn_server["vpn-server"]:::vpn_server_c
  vpn_ssh_config["vpn-ssh-config"]:::vpn_ssh_config_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_secondpc --> den__batteries__define_user
  default_host_secondpc --> extra_registry
  default_host_secondpc --> home_base
  default_host_secondpc --> host__resolve_default_
  default_host_secondpc --> den__batteries__hostname
  default_host_secondpc --> den__batteries__inputs_
  default_host_secondpc --> insecure_predicate
  default_host_secondpc --> lib
  default_host_secondpc --> nix
  default_host_secondpc --> overlays
  default_host_secondpc --> den__batteries__self_
  default_host_secondpc --> den__batteries__sources
  default_host_secondpc --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__auscyber_secondpc
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_secondpc --> fonts___when__4
  fonts_host_secondpc --> fonts___when__5
  home_base --> shell_host_secondpc
  host --> darwin_base
  host --> default_host_secondpc
  host --> fonts_host_secondpc
  host --> host__resolve_host_
  host --> nixos_general
  host --> nixpkgs_config_host_secondpc
  host --> openssh
  host --> pam_rssh_host_secondpc
  host --> secondpc
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  nginx_host_secondpc --> host__resolve_nginx_
  nix --> nix___when__4
  nix --> nix___when__5
  nix___when__4 --> host__resolve__when__4_
  nix___when__5 -.-x host__resolve__when__5_
  nixpkgs_config_host_secondpc --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  secondpc --> builder_server
  secondpc --> builders
  secondpc --> disko
  secondpc --> facter
  secondpc --> local
  secondpc --> nginx_host_secondpc
  secondpc --> nix
  secondpc --> searchix
  secondpc --> secondpc_web
  secondpc --> den__provides__unfree_intel_ocl_
  secondpc --> user_pwd_host_secondpc
  secondpc --> vpn_server
  shell_host_secondpc --> jujutsu
  shell_host_secondpc --> nix_index
  shell_host_secondpc --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> host__resolve_vpn_
  vpn --> vpn_secrets
  vpn --> vpn_ssh_config
  vpn_secrets --> agenix_rekey_host_secondpc
  vpn_server --> vpn
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_auscyber__to_hosts__2_["<policy:auscyber/to-hosts>[2]"]:::_policy_auscyber__to_hosts__2__c
  _policy_default__to_hosts__3_["<policy:default/to-hosts>[3]"]:::_policy_default__to_hosts__3__c
  _policy_hm_user_detect__5_["<policy:hm-user-detect>[5]"]:::_policy_hm_user_detect__5__c
  _policy_main_ssh_key__to_hosts__7_["<policy:main-ssh-key/to-hosts>[7]"]:::_policy_main_ssh_key__to_hosts__7__c
  _policy_nixvim_user_forward__9_["<policy:nixvim-user-forward>[9]"]:::_policy_nixvim_user_forward__9__c
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber__secondpc["auscyber/secondpc"]:::auscyber__secondpc_c
  auscyber__to_hosts["auscyber/to-hosts"]:::auscyber__to_hosts_c
  celler["celler"]:::celler_c
  celler_push["celler-push"]:::celler_push_c
  default_user_auscyber["default"]:::default_user_auscyber_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  fish["fish"]:::fish_c
  fish___anon__4__to_hosts["fish/<anon>:4/to-hosts"]:::fish___anon__4__to_hosts_c
  fonts_user_auscyber["fonts"]:::fonts_user_auscyber_c
  gpg["gpg"]:::gpg_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  kind_system_routes_user_auscyber["kind-system-routes"]:::kind_system_routes_user_auscyber_c
  lix["lix"]:::lix_c
  packages__lspmux[/"packages/lspmux"\]:::packages__lspmux_c
  lspmux["lspmux"]:::lspmux_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nginx_user_auscyber["nginx"]:::nginx_user_auscyber_c
  nh_env["nh-env"]:::nh_env_c
  nix_to_host_user_auscyber["nix-to-host"]:::nix_to_host_user_auscyber_c
  nix__secondpc["nix/secondpc"]:::nix__secondpc_c
  nixpkgs_config_user_auscyber["nixpkgs-config"]:::nixpkgs_config_user_auscyber_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_auscyber["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_auscyber_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  os_to_host_user_auscyber["os-to-host"]:::os_to_host_user_auscyber_c
  overlays_to__overlays_user_auscyber["overlays-to-_overlays"]:::overlays_to__overlays_user_auscyber_c
  overlays_to_flake_parts_user_auscyber["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_auscyber_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  pipe_unfree_user_auscyber["pipe-unfree"]:::pipe_unfree_user_auscyber_c
  den__batteries__primary_user_auscyber_secondpc_{{"batteries/primary-user(auscyber@secondpc)"}}:::den__batteries__primary_user_auscyber_secondpc__c
  route_casks_user_auscyber["route-casks"]:::route_casks_user_auscyber_c
  rust["rust"]:::rust_c
  shell_user_auscyber["shell"]:::shell_user_auscyber_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_castlabs_electron_{{"provides/unfree(castlabs-electron)"}}:::den__provides__unfree_castlabs_electron__c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  user["user"]:::user_c
  user_pwd_user_auscyber["user-pwd"]:::user_pwd_user_auscyber_c
  user_shell__auscyber_secondpc{{"user-shell/auscyber@secondpc"}}:::user_shell__auscyber_secondpc_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_lspmux_{{"user/resolve(lspmux)"}}:::user__resolve_lspmux__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  wakatime["wakatime"]:::wakatime_c
  _policy_auscyber__to_hosts__2_ --> user_pwd_user_auscyber
  auscyber --> celler_push
  auscyber --> fish
  auscyber --> lix
  auscyber --> den__provides__unfree_castlabs_electron_
  auscyber__secondpc --> gpg
  auscyber__secondpc --> neovim
  auscyber__secondpc --> den__batteries__primary_user_auscyber_secondpc_
  celler_push --> agenix_rekey_user_auscyber
  fish --> shell_user_auscyber
  fish --> user_shell__auscyber_secondpc
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  lspmux --> packages__lspmux
  lspmux --> user__resolve__anon__4_
  lspmux --> user__resolve_lspmux_
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  neovim --> stylix
  nix__secondpc --> celler
  nix__secondpc --> nginx_user_auscyber
  nixvim --> packages__eagle_nvim
  nixvim --> rust
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  rust --> lspmux
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_auscyber__to_hosts__2_
  user --> _policy_default__to_hosts__3_
  user --> _policy_hm_user_detect__5_
  user --> _policy_main_ssh_key__to_hosts__7_
  user --> _policy_nixvim_user_forward__9_
  user --> auscyber
  user --> default_user_auscyber
  user --> fonts_user_auscyber
  user --> ivy_fetch
  user --> main_ssh_key
  user --> nixpkgs_config_user_auscyber
  user --> auscyber__secondpc
  user --> nix__secondpc
  user --> shell__to_users
  user --> user__resolve_user_
  end

  neovim -.->|provides| neovim__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber__to_hosts__2__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__3__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__5__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__7__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef _policy_nixvim_user_forward__9__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_vpn_ssh_config__to_users__1__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber__secondpc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef builder_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef builders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef celler_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__auscyber_secondpc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef disko_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef facter_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fish___anon__4__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nginx__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_vpn__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_secondpc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_auscyber_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lix_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef local_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef packages__lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nginx_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nginx_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_to_host_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nix___when__5_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix__secondpc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_to__overlays_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_auscyber_secondpc__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef route_casks_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef searchix_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef secondpc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef secondpc_web_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_castlabs_electron__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_intel_ocl__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_pwd_host_secondpc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef user_pwd_user_auscyber_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef user_shell__auscyber_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_lspmux__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef vpn_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef vpn_ssh_config_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_secondpc fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_auscyber fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## surfacelaptop

**Architecture:** `x86_64-linux`
**Roles:** gui, dev

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  surfacelaptop([surfacelaptop]):::root

  subgraph ctx_host_surfacelaptop["host: surfacelaptop"]
  _anon_["<anon>"]:::_anon__c
  _policy_darwin_base_role_dev__0_["<policy:darwin-base-role-dev>[0]"]:::_policy_darwin_base_role_dev__0__c
  _policy_darwin_base_role_gui__1_["<policy:darwin-base-role-gui>[1]"]:::_policy_darwin_base_role_gui__1__c
  _policy_darwin_finder_role_dev__2_["<policy:darwin-finder-role-dev>[2]"]:::_policy_darwin_finder_role_dev__2__c
  _policy_darwin_finder_role_gui__3_["<policy:darwin-finder-role-gui>[3]"]:::_policy_darwin_finder_role_gui__3__c
  _policy_darwin_general_role_dev__4_["<policy:darwin-general-role-dev>[4]"]:::_policy_darwin_general_role_dev__4__c
  _policy_darwin_general_role_gui__5_["<policy:darwin-general-role-gui>[5]"]:::_policy_darwin_general_role_gui__5__c
  _policy_darwin_hmApps_role_dev__6_["<policy:darwin-hmApps-role-dev>[6]"]:::_policy_darwin_hmApps_role_dev__6__c
  _policy_darwin_hmApps_role_gui__7_["<policy:darwin-hmApps-role-gui>[7]"]:::_policy_darwin_hmApps_role_gui__7__c
  _policy_eagle_nvim_role_dev__8_["<policy:eagle-nvim-role-dev>[8]"]:::_policy_eagle_nvim_role_dev__8__c
  _policy_eagle_nvim_role_gui__9_["<policy:eagle-nvim-role-gui>[9]"]:::_policy_eagle_nvim_role_gui__9__c
  _policy_gpg_role_dev__10_["<policy:gpg-role-dev>[10]"]:::_policy_gpg_role_dev__10__c
  _policy_gpg_role_gui__11_["<policy:gpg-role-gui>[11]"]:::_policy_gpg_role_gui__11__c
  _policy_lspmux_role_dev__12_["<policy:lspmux-role-dev>[12]"]:::_policy_lspmux_role_dev__12__c
  _policy_lspmux_role_gui__13_["<policy:lspmux-role-gui>[13]"]:::_policy_lspmux_role_gui__13__c
  _policy_neovim_role_dev__14_["<policy:neovim-role-dev>[14]"]:::_policy_neovim_role_dev__14__c
  _policy_neovim_role_gui__15_["<policy:neovim-role-gui>[15]"]:::_policy_neovim_role_gui__15__c
  _policy_nixos_general_role_dev__17_["<policy:nixos-general-role-dev>[17]"]:::_policy_nixos_general_role_dev__17__c
  _policy_nixos_general_role_gui__18_["<policy:nixos-general-role-gui>[18]"]:::_policy_nixos_general_role_gui__18__c
  _policy_nixvim_role_dev__19_["<policy:nixvim-role-dev>[19]"]:::_policy_nixvim_role_dev__19__c
  _policy_nixvim_role_gui__20_["<policy:nixvim-role-gui>[20]"]:::_policy_nixvim_role_gui__20__c
  _policy_openssh_role_dev__21_["<policy:openssh-role-dev>[21]"]:::_policy_openssh_role_dev__21__c
  _policy_openssh_role_gui__22_["<policy:openssh-role-gui>[22]"]:::_policy_openssh_role_gui__22__c
  _policy_pam_touchid_role_dev__23_["<policy:pam-touchid-role-dev>[23]"]:::_policy_pam_touchid_role_dev__23__c
  _policy_pam_touchid_role_gui__24_["<policy:pam-touchid-role-gui>[24]"]:::_policy_pam_touchid_role_gui__24__c
  _policy_rust_role_dev__25_["<policy:rust-role-dev>[25]"]:::_policy_rust_role_dev__25__c
  _policy_rust_role_gui__26_["<policy:rust-role-gui>[26]"]:::_policy_rust_role_gui__26__c
  _policy_stylix_role_dev__27_["<policy:stylix-role-dev>[27]"]:::_policy_stylix_role_dev__27__c
  _policy_stylix_role_gui__28_["<policy:stylix-role-gui>[28]"]:::_policy_stylix_role_gui__28__c
  _policy_surfacelaptop_role_dev__29_["<policy:surfacelaptop-role-dev>[29]"]:::_policy_surfacelaptop_role_dev__29__c
  _policy_surfacelaptop_role_gui__30_["<policy:surfacelaptop-role-gui>[30]"]:::_policy_surfacelaptop_role_gui__30__c
  _policy_to_users_role_dev__31_["<policy:to-users-role-dev>[31]"]:::_policy_to_users_role_dev__31__c
  _policy_to_users_role_gui__32_["<policy:to-users-role-gui>[32]"]:::_policy_to_users_role_gui__32__c
  _policy_user_pwd_role_dev__33_["<policy:user-pwd-role-dev>[33]"]:::_policy_user_pwd_role_dev__33__c
  _policy_user_pwd_role_gui__34_["<policy:user-pwd-role-gui>[34]"]:::_policy_user_pwd_role_gui__34__c
  _policy_wakatime_role_dev__35_["<policy:wakatime-role-dev>[35]"]:::_policy_wakatime_role_dev__35__c
  _policy_wakatime_role_gui__36_["<policy:wakatime-role-gui>[36]"]:::_policy_wakatime_role_gui__36__c
  agenix_rekey_host_surfacelaptop["agenix-rekey"]:::agenix_rekey_host_surfacelaptop_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_surfacelaptop["default"]:::default_host_surfacelaptop_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__auscyber_surfacelaptop{{"batteries/define-user/auscyber@surfacelaptop"}}:::den__batteries__define_user__auscyber_surfacelaptop_c
  difftastic["difftastic"]:::difftastic_c
  extra_registry["extra-registry"]:::extra_registry_c
  fonts_host_surfacelaptop["fonts"]:::fonts_host_surfacelaptop_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__4_["host/resolve(<when>:4)"]:::host__resolve__when__4__c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_surfacelaptop["kind-system-routes"]:::kind_system_routes_host_surfacelaptop_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_surfacelaptop["nix-to-host"]:::nix_to_host_host_surfacelaptop_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nix___when__5["nix/<when>:5"]:::nix___when__5_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_surfacelaptop["nixpkgs-config"]:::nixpkgs_config_host_surfacelaptop_c
  nixvim_include_global_pkgs_host_surfacelaptop["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_surfacelaptop_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_surfacelaptop["os-to-host"]:::os_to_host_host_surfacelaptop_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_surfacelaptop["overlays-to-_overlays"]:::overlays_to__overlays_host_surfacelaptop_c
  overlays_to_flake_parts_host_surfacelaptop["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_surfacelaptop_c
  pam_rssh_host_surfacelaptop["pam-rssh"]:::pam_rssh_host_surfacelaptop_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_surfacelaptop["pipe-unfree"]:::pipe_unfree_host_surfacelaptop_c
  route_casks_host_surfacelaptop["route-casks"]:::route_casks_host_surfacelaptop_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_surfacelaptop["shell"]:::shell_host_surfacelaptop_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_surfacelaptop --> agenix_rekey_host_surfacelaptop
  default_host_surfacelaptop --> den__batteries__define_user
  default_host_surfacelaptop --> extra_registry
  default_host_surfacelaptop --> home_base
  default_host_surfacelaptop --> host__resolve_default_
  default_host_surfacelaptop --> den__batteries__hostname
  default_host_surfacelaptop --> den__batteries__inputs_
  default_host_surfacelaptop --> insecure_predicate
  default_host_surfacelaptop --> lib
  default_host_surfacelaptop --> nix
  default_host_surfacelaptop --> overlays
  default_host_surfacelaptop --> den__batteries__self_
  default_host_surfacelaptop --> den__batteries__sources
  default_host_surfacelaptop --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__auscyber_surfacelaptop
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_surfacelaptop --> fonts___when__4
  fonts_host_surfacelaptop --> fonts___when__5
  home_base --> shell_host_surfacelaptop
  host --> darwin_base
  host --> default_host_surfacelaptop
  host --> fonts_host_surfacelaptop
  host --> host__resolve_host_
  host --> nixos_general
  host --> nixpkgs_config_host_surfacelaptop
  host --> openssh
  host --> pam_rssh_host_surfacelaptop
  host --> surfacelaptop
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  nix --> nix___when__4
  nix --> nix___when__5
  nix___when__4 --> host__resolve__when__4_
  nix___when__5 -.-x host__resolve__when__5_
  nixpkgs_config_host_surfacelaptop --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_surfacelaptop --> jujutsu
  shell_host_surfacelaptop --> nix_index
  shell_host_surfacelaptop --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  _policy_agenix_rekey_role_dev__0_["<policy:agenix-rekey-role-dev>[0]"]:::_policy_agenix_rekey_role_dev__0__c
  _policy_agenix_rekey_role_gui__1_["<policy:agenix-rekey-role-gui>[1]"]:::_policy_agenix_rekey_role_gui__1__c
  _policy_agenix_rekey__to_users__2_["<policy:agenix-rekey/to-users>[2]"]:::_policy_agenix_rekey__to_users__2__c
  _policy_auscyber_role_dev__3_["<policy:auscyber-role-dev>[3]"]:::_policy_auscyber_role_dev__3__c
  _policy_auscyber_role_gui__4_["<policy:auscyber-role-gui>[4]"]:::_policy_auscyber_role_gui__4__c
  _policy_auscyber__to_hosts__6_["<policy:auscyber/to-hosts>[6]"]:::_policy_auscyber__to_hosts__6__c
  _policy_celler_push_role_dev__7_["<policy:celler-push-role-dev>[7]"]:::_policy_celler_push_role_dev__7__c
  _policy_celler_push_role_gui__8_["<policy:celler-push-role-gui>[8]"]:::_policy_celler_push_role_gui__8__c
  _policy_default_role_dev__9_["<policy:default-role-dev>[9]"]:::_policy_default_role_dev__9__c
  _policy_default_role_gui__10_["<policy:default-role-gui>[10]"]:::_policy_default_role_gui__10__c
  _policy_default__to_hosts__11_["<policy:default/to-hosts>[11]"]:::_policy_default__to_hosts__11__c
  _policy_define_user_role_dev__12_["<policy:define-user-role-dev>[12]"]:::_policy_define_user_role_dev__12__c
  _policy_define_user_role_gui__13_["<policy:define-user-role-gui>[13]"]:::_policy_define_user_role_gui__13__c
  _policy_difftastic_role_dev__14_["<policy:difftastic-role-dev>[14]"]:::_policy_difftastic_role_dev__14__c
  _policy_difftastic_role_gui__15_["<policy:difftastic-role-gui>[15]"]:::_policy_difftastic_role_gui__15__c
  _policy_extra_registry_role_dev__16_["<policy:extra-registry-role-dev>[16]"]:::_policy_extra_registry_role_dev__16__c
  _policy_extra_registry_role_gui__17_["<policy:extra-registry-role-gui>[17]"]:::_policy_extra_registry_role_gui__17__c
  _policy_fish_role_dev__18_["<policy:fish-role-dev>[18]"]:::_policy_fish_role_dev__18__c
  _policy_fish_role_gui__19_["<policy:fish-role-gui>[19]"]:::_policy_fish_role_gui__19__c
  _policy_fonts_role_dev__21_["<policy:fonts-role-dev>[21]"]:::_policy_fonts_role_dev__21__c
  _policy_fonts_role_gui__22_["<policy:fonts-role-gui>[22]"]:::_policy_fonts_role_gui__22__c
  _policy_hm_user_detect__23_["<policy:hm-user-detect>[23]"]:::_policy_hm_user_detect__23__c
  _policy_home_base_role_dev__25_["<policy:home-base-role-dev>[25]"]:::_policy_home_base_role_dev__25__c
  _policy_home_base_role_gui__26_["<policy:home-base-role-gui>[26]"]:::_policy_home_base_role_gui__26__c
  _policy_hostname_role_dev__27_["<policy:hostname-role-dev>[27]"]:::_policy_hostname_role_dev__27__c
  _policy_hostname_role_gui__28_["<policy:hostname-role-gui>[28]"]:::_policy_hostname_role_gui__28__c
  _policy_inputs__role_dev__29_["<policy:inputs'-role-dev>[29]"]:::_policy_inputs__role_dev__29__c
  _policy_inputs__role_gui__30_["<policy:inputs'-role-gui>[30]"]:::_policy_inputs__role_gui__30__c
  _policy_insecure_predicate_role_dev__31_["<policy:insecure-predicate-role-dev>[31]"]:::_policy_insecure_predicate_role_dev__31__c
  _policy_insecure_predicate_role_gui__32_["<policy:insecure-predicate-role-gui>[32]"]:::_policy_insecure_predicate_role_gui__32__c
  _policy_ivy_fetch_role_dev__33_["<policy:ivy-fetch-role-dev>[33]"]:::_policy_ivy_fetch_role_dev__33__c
  _policy_ivy_fetch_role_gui__34_["<policy:ivy-fetch-role-gui>[34]"]:::_policy_ivy_fetch_role_gui__34__c
  _policy_jujutsu_role_dev__35_["<policy:jujutsu-role-dev>[35]"]:::_policy_jujutsu_role_dev__35__c
  _policy_jujutsu_role_gui__36_["<policy:jujutsu-role-gui>[36]"]:::_policy_jujutsu_role_gui__36__c
  _policy_lib_role_dev__37_["<policy:lib-role-dev>[37]"]:::_policy_lib_role_dev__37__c
  _policy_lib_role_gui__38_["<policy:lib-role-gui>[38]"]:::_policy_lib_role_gui__38__c
  _policy_lix_role_dev__39_["<policy:lix-role-dev>[39]"]:::_policy_lix_role_dev__39__c
  _policy_lix_role_gui__40_["<policy:lix-role-gui>[40]"]:::_policy_lix_role_gui__40__c
  _policy_main_ssh_key_role_dev__41_["<policy:main-ssh-key-role-dev>[41]"]:::_policy_main_ssh_key_role_dev__41__c
  _policy_main_ssh_key_role_gui__42_["<policy:main-ssh-key-role-gui>[42]"]:::_policy_main_ssh_key_role_gui__42__c
  _policy_main_ssh_key__to_hosts__43_["<policy:main-ssh-key/to-hosts>[43]"]:::_policy_main_ssh_key__to_hosts__43__c
  _policy_nix_index_role_dev__44_["<policy:nix-index-role-dev>[44]"]:::_policy_nix_index_role_dev__44__c
  _policy_nix_index_role_gui__45_["<policy:nix-index-role-gui>[45]"]:::_policy_nix_index_role_gui__45__c
  _policy_nix_role_dev__46_["<policy:nix-role-dev>[46]"]:::_policy_nix_role_dev__46__c
  _policy_nix_role_gui__47_["<policy:nix-role-gui>[47]"]:::_policy_nix_role_gui__47__c
  _policy_nixpkgs_config_role_dev__48_["<policy:nixpkgs-config-role-dev>[48]"]:::_policy_nixpkgs_config_role_dev__48__c
  _policy_nixpkgs_config_role_gui__49_["<policy:nixpkgs-config-role-gui>[49]"]:::_policy_nixpkgs_config_role_gui__49__c
  _policy_nixvim_user_forward__50_["<policy:nixvim-user-forward>[50]"]:::_policy_nixvim_user_forward__50__c
  _policy_overlays_role_dev__51_["<policy:overlays-role-dev>[51]"]:::_policy_overlays_role_dev__51__c
  _policy_overlays_role_gui__52_["<policy:overlays-role-gui>[52]"]:::_policy_overlays_role_gui__52__c
  _policy_pam_rssh_role_dev__53_["<policy:pam-rssh-role-dev>[53]"]:::_policy_pam_rssh_role_dev__53__c
  _policy_pam_rssh_role_gui__54_["<policy:pam-rssh-role-gui>[54]"]:::_policy_pam_rssh_role_gui__54__c
  _policy_self__role_dev__55_["<policy:self'-role-dev>[55]"]:::_policy_self__role_dev__55__c
  _policy_self__role_gui__56_["<policy:self'-role-gui>[56]"]:::_policy_self__role_gui__56__c
  _policy_shell_role_dev__57_["<policy:shell-role-dev>[57]"]:::_policy_shell_role_dev__57__c
  _policy_shell_role_gui__58_["<policy:shell-role-gui>[58]"]:::_policy_shell_role_gui__58__c
  _policy_sources_role_dev__60_["<policy:sources-role-dev>[60]"]:::_policy_sources_role_dev__60__c
  _policy_sources_role_gui__61_["<policy:sources-role-gui>[61]"]:::_policy_sources_role_gui__61__c
  _policy_starship_role_dev__62_["<policy:starship-role-dev>[62]"]:::_policy_starship_role_dev__62__c
  _policy_starship_role_gui__63_["<policy:starship-role-gui>[63]"]:::_policy_starship_role_gui__63__c
  _policy_unfree_predicate_role_dev__64_["<policy:unfree-predicate-role-dev>[64]"]:::_policy_unfree_predicate_role_dev__64__c
  _policy_unfree_predicate_role_gui__65_["<policy:unfree-predicate-role-gui>[65]"]:::_policy_unfree_predicate_role_gui__65__c
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  agenix_rekey_role_dev["agenix-rekey-role-dev"]:::agenix_rekey_role_dev_c
  agenix_rekey_role_gui["agenix-rekey-role-gui"]:::agenix_rekey_role_gui_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber_role_dev["auscyber-role-dev"]:::auscyber_role_dev_c
  auscyber_role_gui["auscyber-role-gui"]:::auscyber_role_gui_c
  auscyber__surfacelaptop["auscyber/surfacelaptop"]:::auscyber__surfacelaptop_c
  auscyber__to_hosts["auscyber/to-hosts"]:::auscyber__to_hosts_c
  celler_push["celler-push"]:::celler_push_c
  celler_push_role_dev["celler-push-role-dev"]:::celler_push_role_dev_c
  celler_push_role_gui["celler-push-role-gui"]:::celler_push_role_gui_c
  default_user_auscyber["default"]:::default_user_auscyber_c
  default_role_dev["default-role-dev"]:::default_role_dev_c
  default_role_gui["default-role-gui"]:::default_role_gui_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  define_user_role_dev["define-user-role-dev"]:::define_user_role_dev_c
  define_user_role_gui["define-user-role-gui"]:::define_user_role_gui_c
  difftastic_role_dev["difftastic-role-dev"]:::difftastic_role_dev_c
  difftastic_role_gui["difftastic-role-gui"]:::difftastic_role_gui_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  extra_registry_role_dev["extra-registry-role-dev"]:::extra_registry_role_dev_c
  extra_registry_role_gui["extra-registry-role-gui"]:::extra_registry_role_gui_c
  fish["fish"]:::fish_c
  fish_role_dev["fish-role-dev"]:::fish_role_dev_c
  fish_role_gui["fish-role-gui"]:::fish_role_gui_c
  fish___anon__4__to_hosts["fish/<anon>:4/to-hosts"]:::fish___anon__4__to_hosts_c
  fonts_user_auscyber["fonts"]:::fonts_user_auscyber_c
  fonts_role_dev["fonts-role-dev"]:::fonts_role_dev_c
  fonts_role_gui["fonts-role-gui"]:::fonts_role_gui_c
  gpg["gpg"]:::gpg_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  home_base_role_dev["home-base-role-dev"]:::home_base_role_dev_c
  home_base_role_gui["home-base-role-gui"]:::home_base_role_gui_c
  hostname_role_dev["hostname-role-dev"]:::hostname_role_dev_c
  hostname_role_gui["hostname-role-gui"]:::hostname_role_gui_c
  inputs__role_dev["inputs'-role-dev"]:::inputs__role_dev_c
  inputs__role_gui["inputs'-role-gui"]:::inputs__role_gui_c
  insecure_predicate_role_dev["insecure-predicate-role-dev"]:::insecure_predicate_role_dev_c
  insecure_predicate_role_gui["insecure-predicate-role-gui"]:::insecure_predicate_role_gui_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy_fetch_role_dev["ivy-fetch-role-dev"]:::ivy_fetch_role_dev_c
  ivy_fetch_role_gui["ivy-fetch-role-gui"]:::ivy_fetch_role_gui_c
  jujutsu_role_dev["jujutsu-role-dev"]:::jujutsu_role_dev_c
  jujutsu_role_gui["jujutsu-role-gui"]:::jujutsu_role_gui_c
  kind_system_routes_user_auscyber["kind-system-routes"]:::kind_system_routes_user_auscyber_c
  lib_role_dev["lib-role-dev"]:::lib_role_dev_c
  lib_role_gui["lib-role-gui"]:::lib_role_gui_c
  lix["lix"]:::lix_c
  lix_role_dev["lix-role-dev"]:::lix_role_dev_c
  lix_role_gui["lix-role-gui"]:::lix_role_gui_c
  packages__lspmux[/"packages/lspmux"\]:::packages__lspmux_c
  lspmux["lspmux"]:::lspmux_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key_role_dev["main-ssh-key-role-dev"]:::main_ssh_key_role_dev_c
  main_ssh_key_role_gui["main-ssh-key-role-gui"]:::main_ssh_key_role_gui_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nix_index_role_dev["nix-index-role-dev"]:::nix_index_role_dev_c
  nix_index_role_gui["nix-index-role-gui"]:::nix_index_role_gui_c
  nix_role_dev["nix-role-dev"]:::nix_role_dev_c
  nix_role_gui["nix-role-gui"]:::nix_role_gui_c
  nix_to_host_user_auscyber["nix-to-host"]:::nix_to_host_user_auscyber_c
  nixpkgs_config_user_auscyber["nixpkgs-config"]:::nixpkgs_config_user_auscyber_c
  nixpkgs_config_role_dev["nixpkgs-config-role-dev"]:::nixpkgs_config_role_dev_c
  nixpkgs_config_role_gui["nixpkgs-config-role-gui"]:::nixpkgs_config_role_gui_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_auscyber["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_auscyber_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  os_to_host_user_auscyber["os-to-host"]:::os_to_host_user_auscyber_c
  overlays_role_dev["overlays-role-dev"]:::overlays_role_dev_c
  overlays_role_gui["overlays-role-gui"]:::overlays_role_gui_c
  overlays_to__overlays_user_auscyber["overlays-to-_overlays"]:::overlays_to__overlays_user_auscyber_c
  overlays_to_flake_parts_user_auscyber["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_auscyber_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  pam_rssh_role_dev["pam-rssh-role-dev"]:::pam_rssh_role_dev_c
  pam_rssh_role_gui["pam-rssh-role-gui"]:::pam_rssh_role_gui_c
  pipe_unfree_user_auscyber["pipe-unfree"]:::pipe_unfree_user_auscyber_c
  den__batteries__primary_user_auscyber_surfacelaptop_{{"batteries/primary-user(auscyber@surfacelaptop)"}}:::den__batteries__primary_user_auscyber_surfacelaptop__c
  route_casks_user_auscyber["route-casks"]:::route_casks_user_auscyber_c
  rust["rust"]:::rust_c
  self__role_dev["self'-role-dev"]:::self__role_dev_c
  self__role_gui["self'-role-gui"]:::self__role_gui_c
  shell_user_auscyber["shell"]:::shell_user_auscyber_c
  shell_role_dev["shell-role-dev"]:::shell_role_dev_c
  shell_role_gui["shell-role-gui"]:::shell_role_gui_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  sources_role_dev["sources-role-dev"]:::sources_role_dev_c
  sources_role_gui["sources-role-gui"]:::sources_role_gui_c
  starship_role_dev["starship-role-dev"]:::starship_role_dev_c
  starship_role_gui["starship-role-gui"]:::starship_role_gui_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_castlabs_electron_{{"provides/unfree(castlabs-electron)"}}:::den__provides__unfree_castlabs_electron__c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  unfree_predicate_role_dev["unfree-predicate-role-dev"]:::unfree_predicate_role_dev_c
  unfree_predicate_role_gui["unfree-predicate-role-gui"]:::unfree_predicate_role_gui_c
  user["user"]:::user_c
  user_pwd["user-pwd"]:::user_pwd_c
  user_shell__auscyber_surfacelaptop{{"user-shell/auscyber@surfacelaptop"}}:::user_shell__auscyber_surfacelaptop_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_lspmux_{{"user/resolve(lspmux)"}}:::user__resolve_lspmux__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  wakatime["wakatime"]:::wakatime_c
  _policy_auscyber__to_hosts__6_ --> user_pwd
  auscyber --> celler_push
  auscyber --> fish
  auscyber --> lix
  auscyber --> den__provides__unfree_castlabs_electron_
  auscyber__surfacelaptop --> gpg
  auscyber__surfacelaptop --> neovim
  auscyber__surfacelaptop --> den__batteries__primary_user_auscyber_surfacelaptop_
  celler_push --> agenix_rekey_user_auscyber
  fish --> shell_user_auscyber
  fish --> user_shell__auscyber_surfacelaptop
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  lspmux --> packages__lspmux
  lspmux --> user__resolve__anon__4_
  lspmux --> user__resolve_lspmux_
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> rust
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  rust --> lspmux
  user --> _policy_agenix_rekey_role_dev__0_
  user --> _policy_agenix_rekey_role_gui__1_
  user --> _policy_agenix_rekey__to_users__2_
  user --> _policy_auscyber_role_dev__3_
  user --> _policy_auscyber_role_gui__4_
  user --> _policy_auscyber__to_hosts__6_
  user --> _policy_celler_push_role_dev__7_
  user --> _policy_celler_push_role_gui__8_
  user --> _policy_default_role_dev__9_
  user --> _policy_default_role_gui__10_
  user --> _policy_default__to_hosts__11_
  user --> _policy_define_user_role_dev__12_
  user --> _policy_define_user_role_gui__13_
  user --> _policy_difftastic_role_dev__14_
  user --> _policy_difftastic_role_gui__15_
  user --> _policy_extra_registry_role_dev__16_
  user --> _policy_extra_registry_role_gui__17_
  user --> _policy_fish_role_dev__18_
  user --> _policy_fish_role_gui__19_
  user --> _policy_fonts_role_dev__21_
  user --> _policy_fonts_role_gui__22_
  user --> _policy_hm_user_detect__23_
  user --> _policy_home_base_role_dev__25_
  user --> _policy_home_base_role_gui__26_
  user --> _policy_hostname_role_dev__27_
  user --> _policy_hostname_role_gui__28_
  user --> _policy_inputs__role_dev__29_
  user --> _policy_inputs__role_gui__30_
  user --> _policy_insecure_predicate_role_dev__31_
  user --> _policy_insecure_predicate_role_gui__32_
  user --> _policy_ivy_fetch_role_dev__33_
  user --> _policy_ivy_fetch_role_gui__34_
  user --> _policy_jujutsu_role_dev__35_
  user --> _policy_jujutsu_role_gui__36_
  user --> _policy_lib_role_dev__37_
  user --> _policy_lib_role_gui__38_
  user --> _policy_lix_role_dev__39_
  user --> _policy_lix_role_gui__40_
  user --> _policy_main_ssh_key_role_dev__41_
  user --> _policy_main_ssh_key_role_gui__42_
  user --> _policy_main_ssh_key__to_hosts__43_
  user --> _policy_nix_index_role_dev__44_
  user --> _policy_nix_index_role_gui__45_
  user --> _policy_nix_role_dev__46_
  user --> _policy_nix_role_gui__47_
  user --> _policy_nixpkgs_config_role_dev__48_
  user --> _policy_nixpkgs_config_role_gui__49_
  user --> _policy_nixvim_user_forward__50_
  user --> _policy_overlays_role_dev__51_
  user --> _policy_overlays_role_gui__52_
  user --> _policy_pam_rssh_role_dev__53_
  user --> _policy_pam_rssh_role_gui__54_
  user --> _policy_self__role_dev__55_
  user --> _policy_self__role_gui__56_
  user --> _policy_shell_role_dev__57_
  user --> _policy_shell_role_gui__58_
  user --> _policy_sources_role_dev__60_
  user --> _policy_sources_role_gui__61_
  user --> _policy_starship_role_dev__62_
  user --> _policy_starship_role_gui__63_
  user --> _policy_unfree_predicate_role_dev__64_
  user --> _policy_unfree_predicate_role_gui__65_
  user --> auscyber
  user --> default_user_auscyber
  user --> fonts_user_auscyber
  user --> ivy_fetch
  user --> main_ssh_key
  user --> nixpkgs_config_user_auscyber
  user --> auscyber__surfacelaptop
  user --> shell__to_users
  user --> user__resolve_user_
  end

  neovim -.->|provides| neovim__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey_role_dev__0__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey_role_gui__1__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_agenix_rekey__to_users__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber_role_dev__3__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber_role_gui__4__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber__to_hosts__6__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_dev__7__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_celler_push_role_gui__8__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_dev__0__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_base_role_gui__1__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_dev__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_finder_role_gui__3__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_dev__4__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_general_role_gui__5__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_dev__6__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_darwin_hmApps_role_gui__7__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default_role_dev__9__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default_role_gui__10__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__11__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_dev__12__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_define_user_role_gui__13__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_dev__14__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_difftastic_role_gui__15__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_dev__8__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_eagle_nvim_role_gui__9__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_dev__16__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_extra_registry_role_gui__17__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_dev__18__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fish_role_gui__19__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_dev__21__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_fonts_role_gui__22__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_dev__10__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_gpg_role_gui__11__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__23__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_dev__25__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_home_base_role_gui__26__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_dev__27__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hostname_role_gui__28__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_dev__29__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_inputs__role_gui__30__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_dev__31__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_insecure_predicate_role_gui__32__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_dev__33__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_ivy_fetch_role_gui__34__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_dev__35__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_jujutsu_role_gui__36__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_dev__37__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lib_role_gui__38__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lix_role_dev__39__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lix_role_gui__40__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_dev__12__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_lspmux_role_gui__13__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_dev__41__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key_role_gui__42__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__43__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef _policy_neovim_role_dev__14__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_neovim_role_gui__15__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_dev__44__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_index_role_gui__45__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_dev__46__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nix_role_gui__47__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_dev__17__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixos_general_role_gui__18__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_dev__48__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixpkgs_config_role_gui__49__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_dev__19__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_role_gui__20__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_nixvim_user_forward__50__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_dev__21__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_openssh_role_gui__22__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_dev__51__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_overlays_role_gui__52__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_dev__53__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_rssh_role_gui__54__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_dev__23__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_pam_touchid_role_gui__24__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_dev__25__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_rust_role_gui__26__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_dev__55__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_self__role_gui__56__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_dev__57__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_shell_role_gui__58__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_dev__60__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_sources_role_gui__61__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_dev__62__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_starship_role_gui__63__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_dev__27__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_stylix_role_gui__28__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_surfacelaptop_role_dev__29__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_surfacelaptop_role_gui__30__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_dev__31__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_to_users_role_gui__32__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_dev__64__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_unfree_predicate_role_gui__65__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_user_pwd_role_dev__33__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_user_pwd_role_gui__34__c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_dev__35__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_wakatime_role_gui__36__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber__surfacelaptop_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef celler_push_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef celler_push_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef define_user_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef define_user_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user__auscyber_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef difftastic_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef difftastic_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef extra_registry_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef extra_registry_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fish_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fish___anon__4__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef home_base_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef hostname_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef hostname_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef inputs__role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef inputs__role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivy_fetch_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_host_surfacelaptop_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_auscyber_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lix_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef lix_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lix_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef lspmux_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_index_role_dev_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_index_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nix___when__5_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixpkgs_config_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_rssh_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_rssh_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_auscyber_surfacelaptop__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef route_casks_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef rust_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef self__role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef self__role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef sources_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sources_role_gui_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef starship_role_dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef starship_role_gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_castlabs_electron__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate_role_dev_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate_role_gui_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_pwd_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef user_shell__auscyber_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_lspmux__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_surfacelaptop fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_auscyber fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## wsl-nixos

**Architecture:** `x86_64-linux`

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  wsl_nixos([wsl-nixos]):::root

  subgraph ctx_host_wsl_nixos["host: wsl-nixos"]
  _anon_["<anon>"]:::_anon__c
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_wsl_nixos["default"]:::default_host_wsl_nixos_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__nixos_wsl_nixos{{"batteries/define-user/nixos@wsl-nixos"}}:::den__batteries__define_user__nixos_wsl_nixos_c
  difftastic["difftastic"]:::difftastic_c
  extra_registry["extra-registry"]:::extra_registry_c
  fonts_host_wsl_nixos["fonts"]:::fonts_host_wsl_nixos_c
  fonts___when__4["fonts/<when>:4"]:::fonts___when__4_c
  fonts___when__5["fonts/<when>:5"]:::fonts___when__5_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__when__5_["host/resolve(<when>:5)"]:::host__resolve__when__5__c
  host__resolve_default_{{"host/resolve(default)"}}:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_user_["host/resolve(user)"]:::host__resolve_user__c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  den__batteries__inputs___user{{"batteries/inputs'/user"}}:::den__batteries__inputs___user_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  kind_system_routes_host_wsl_nixos["kind-system-routes"]:::kind_system_routes_host_wsl_nixos_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nix_to_host_host_wsl_nixos["nix-to-host"]:::nix_to_host_host_wsl_nixos_c
  nix___when__4["nix/<when>:4"]:::nix___when__4_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_wsl_nixos["nixpkgs-config"]:::nixpkgs_config_host_wsl_nixos_c
  nixvim_include_global_pkgs_host_wsl_nixos["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_host_wsl_nixos_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_wsl_nixos["os-to-host"]:::os_to_host_host_wsl_nixos_c
  overlays["overlays"]:::overlays_c
  overlays_to__overlays_host_wsl_nixos["overlays-to-_overlays"]:::overlays_to__overlays_host_wsl_nixos_c
  overlays_to_flake_parts_host_wsl_nixos["overlays-to-flake-parts"]:::overlays_to_flake_parts_host_wsl_nixos_c
  pam_rssh_host_wsl_nixos["pam-rssh"]:::pam_rssh_host_wsl_nixos_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_wsl_nixos["pipe-unfree"]:::pipe_unfree_host_wsl_nixos_c
  route_casks_host_wsl_nixos["route-casks"]:::route_casks_host_wsl_nixos_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_wsl_nixos["shell"]:::shell_host_wsl_nixos_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_wsl_nixos --> agenix_rekey
  default_host_wsl_nixos --> den__batteries__define_user
  default_host_wsl_nixos --> extra_registry
  default_host_wsl_nixos --> home_base
  default_host_wsl_nixos --> host__resolve_default_
  default_host_wsl_nixos --> den__batteries__hostname
  default_host_wsl_nixos --> den__batteries__inputs_
  default_host_wsl_nixos --> insecure_predicate
  default_host_wsl_nixos --> lib
  default_host_wsl_nixos --> nix
  default_host_wsl_nixos --> overlays
  default_host_wsl_nixos --> den__batteries__self_
  default_host_wsl_nixos --> den__batteries__sources
  default_host_wsl_nixos --> unfree_predicate
  den__batteries__define_user --> den__batteries__define_user__nixos_wsl_nixos
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__inputs_ --> den__batteries__inputs___user
  den__batteries__inputs___user --> host__resolve_user_
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__self_ --> den__batteries__self___user
  den__batteries__self___user --> host__resolve_user_
  den__batteries__sources --> den__batteries__sources__os
  den__batteries__sources --> den__batteries__sources__user
  den__batteries__sources__user --> host__resolve_user_
  difftastic --> host__resolve_difftastic_
  fonts_host_wsl_nixos --> fonts___when__4
  fonts_host_wsl_nixos --> fonts___when__5
  home_base --> shell_host_wsl_nixos
  host --> darwin_base
  host --> default_host_wsl_nixos
  host --> fonts_host_wsl_nixos
  host --> host__resolve_host_
  host --> nixos_general
  host --> nixpkgs_config_host_wsl_nixos
  host --> openssh
  host --> pam_rssh_host_wsl_nixos
  host --> wsl_nixos
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  jujutsu --> difftastic
  nix --> nix___when__4
  nixpkgs_config_host_wsl_nixos --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_wsl_nixos --> jujutsu
  shell_host_wsl_nixos --> nix_index
  shell_host_wsl_nixos --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_nixos["user: nixos"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_default__to_hosts__1_["<policy:default/to-hosts>[1]"]:::_policy_default__to_hosts__1__c
  _policy_hm_user_detect__3_["<policy:hm-user-detect>[3]"]:::_policy_hm_user_detect__3__c
  _policy_main_ssh_key__to_hosts__5_["<policy:main-ssh-key/to-hosts>[5]"]:::_policy_main_ssh_key__to_hosts__5__c
  _policy_nixvim_user_forward__7_["<policy:nixvim-user-forward>[7]"]:::_policy_nixvim_user_forward__7__c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_nixos["default"]:::default_user_nixos_c
  default__to_hosts["default/to-hosts"]:::default__to_hosts_c
  fish["fish"]:::fish_c
  fish___anon__4__to_hosts["fish/<anon>:4/to-hosts"]:::fish___anon__4__to_hosts_c
  fonts_user_nixos["fonts"]:::fonts_user_nixos_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  kind_system_routes_user_nixos["kind-system-routes"]:::kind_system_routes_user_nixos_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  nh_env["nh-env"]:::nh_env_c
  nix_to_host_user_nixos["nix-to-host"]:::nix_to_host_user_nixos_c
  nixos{{"nixos"}}:::nixos_c
  wsl_nixos__nixos[/"wsl-nixos/nixos"\]:::wsl_nixos__nixos_c
  nixos__to_users["nixos/to-users"]:::nixos__to_users_c
  nixpkgs_config_user_nixos["nixpkgs-config"]:::nixpkgs_config_user_nixos_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nixvim_include_global_pkgs_user_nixos["nixvim-include-global-pkgs"]:::nixvim_include_global_pkgs_user_nixos_c
  nixvim_user_forward["nixvim-user-forward"]:::nixvim_user_forward_c
  os_to_host_user_nixos["os-to-host"]:::os_to_host_user_nixos_c
  overlays_to__overlays_user_nixos["overlays-to-_overlays"]:::overlays_to__overlays_user_nixos_c
  overlays_to_flake_parts_user_nixos["overlays-to-flake-parts"]:::overlays_to_flake_parts_user_nixos_c
  pam_rssh_user_nixos["pam-rssh"]:::pam_rssh_user_nixos_c
  pipe_unfree_user_nixos["pipe-unfree"]:::pipe_unfree_user_nixos_c
  den__batteries__primary_user_nixos_wsl_nixos_{{"batteries/primary-user(nixos@wsl-nixos)"}}:::den__batteries__primary_user_nixos_wsl_nixos__c
  route_casks_user_nixos["route-casks"]:::route_casks_user_nixos_c
  shell_user_nixos["shell"]:::shell_user_nixos_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  user["user"]:::user_c
  user_shell__nixos_wsl_nixos{{"user-shell/nixos@wsl-nixos"}}:::user_shell__nixos_wsl_nixos_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__4_["user/resolve(<anon>:4)"]:::user__resolve__anon__4__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  fish --> shell_user_nixos
  fish --> user_shell__nixos_wsl_nixos
  fish --> user__resolve__anon__4_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  main_ssh_key --> pam_rssh_user_nixos
  nixos --> fish
  nixos --> den__batteries__primary_user_nixos_wsl_nixos_
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_default__to_hosts__1_
  user --> _policy_hm_user_detect__3_
  user --> _policy_main_ssh_key__to_hosts__5_
  user --> _policy_nixvim_user_forward__7_
  user --> default_user_nixos
  user --> fonts_user_nixos
  user --> ivy_fetch
  user --> main_ssh_key
  user --> nixos
  user --> wsl_nixos__nixos
  user --> nixpkgs_config_user_nixos
  user --> nixos__to_users
  user --> shell__to_users
  user --> user__resolve_user_
  end

  wsl_nixos -.->|provides| wsl_nixos__nixos

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_default__to_hosts__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__3__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__5__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef _policy_nixvim_user_forward__7__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef default__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__nixos_wsl_nixos_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef extra_registry_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fish___anon__4__to_hosts_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef fonts_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__5_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__when__5__c fill:#4d2d00,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_default__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs___user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef packages__ivy_fetch_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_wsl_nixos_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_nixos_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix_to_host_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_to_host_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix___when__4_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef wsl_nixos__nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos__to_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixpkgs_config_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_host_wsl_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_include_global_pkgs_user_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixvim_user_forward_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_wsl_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef overlays_to__overlays_host_wsl_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to__overlays_user_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_host_wsl_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_to_flake_parts_user_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_nixos_wsl_nixos__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef route_casks_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef route_casks_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_wsl_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__nixos_wsl_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__4__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
style ctx_host_wsl_nixos fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_nixos fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```


## My Packages

| Package | Version | Description |
|---------|---------|-------------|
| cotabby | `v0.6.2-beta` |  |
| ghostty | `1.3.1` | Fast, native, feature-rich terminal emulator pushing modern features |
| helium | `0.14.7.1` | Private, fast, and honest web browser based on Chromium |
| ivy-fetch | `unknown` |  |
| jankyborders | `a7297ca7d1933f3a30b12e8f10750e8d84eeee1e` | Lightweight tool designed to add colored borders to user windows on macOS 14.0+ |
| jj-mcp-server | `1.0.1` | Model Context Protocol server for the Jujutsu (jj) version control system |
| kanata-ls | `701dbf9a10a6857fe9ea72944c84475ab58c81c8` | Kanata Language Server |
| kanata-tray | `v0.8.0` | Tray Icon for Kanata  |
| lspmux | `18861f9d59e74ece8d867772cf07fa302c2dae98` |  |
| proton-ge-bin | `GE-Proton11-1` | Compatibility tool for Steam Play based on Wine and additional components.

(This is intended for use in the `programs.steam.extraCompatPackages` option only.)
 |
| sketchybar | `2.24.0` | Highly customizable macOS status bar replacement |
| zotero-mcp | `0.6.2` | Model Context Protocol server for Zotero |

## Other Projects
- [xmonad](https://github.com/xmonad/xmonad)
- [xmonad-contrib](https://github.com/xmonad/xmonad-contrib)
- [alacritty](https://github.com/alacritty/alacritty)
- [ghostty](https://github.com/ghostty-org/ghostty)
- [neovim](https://github.com/neovim/neovim)
- [starship](https://github.com/starship/starship)
