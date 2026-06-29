# My Dotfiles
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

## Systems

| System | Architecture | Roles |
|--------|--------------|-------|
| [Ivys-MacBook-Pro](#Ivys-MacBook-Pro) | aarch64-darwin | study, gui, dev |
| [auspc](#auspc) | x86_64-linux | gui, gaming, dev |
| [contabo](#contabo) | x86_64-linux |  |
| [imflopet](#imflopet) | x86_64-linux |  |
| [macmini](#macmini) | aarch64-darwin | gui |
| [pentestvm](#pentestvm) | x86_64-linux |  |
| [secondpc](#secondpc) | x86_64-linux |  |
| [surfacelaptop](#surfacelaptop) | x86_64-linux | gui, dev |
| [wsl-nixos](#wsl-nixos) | x86_64-linux |  |

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
  agenix_rekey_host_Ivys_MacBook_Pro["agenix-rekey"]:::agenix_rekey_host_Ivys_MacBook_Pro_c
  builders["builders"]:::builders_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_Ivys_MacBook_Pro["default"]:::default_host_Ivys_MacBook_Pro_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro{{"batteries/define-user/ivypierlot@Ivys-MacBook-Pro"}}:::den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro_c
  difftastic["difftastic"]:::difftastic_c
  fonts_host_Ivys_MacBook_Pro["fonts"]:::fonts_host_Ivys_MacBook_Pro_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  homebrew["homebrew"]:::homebrew_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nix["nix"]:::nix_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_Ivys_MacBook_Pro["nixpkgs-config"]:::nixpkgs_config_host_Ivys_MacBook_Pro_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_Ivys_MacBook_Pro["os-to-host"]:::os_to_host_host_Ivys_MacBook_Pro_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_Ivys_MacBook_Pro["pam-rssh"]:::pam_rssh_host_Ivys_MacBook_Pro_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_Ivys_MacBook_Pro["pipe-unfree"]:::pipe_unfree_host_Ivys_MacBook_Pro_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_Ivys_MacBook_Pro["shell"]:::shell_host_Ivys_MacBook_Pro_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  sudoagents["sudoagents"]:::sudoagents_c
  ivypierlot__Ivys_MacBook_Pro__to_users[/"Ivys-MacBook-Pro/to-users"\]:::ivypierlot__Ivys_MacBook_Pro__to_users_c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  Ivys_MacBook_Pro --> builders
  Ivys_MacBook_Pro --> homebrew
  Ivys_MacBook_Pro --> karabiner_driver
  Ivys_MacBook_Pro --> sudoagents
  Ivys_MacBook_Pro --> vpn
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_Ivys_MacBook_Pro --> den__batteries__define_user
  default_host_Ivys_MacBook_Pro --> home_base
  default_host_Ivys_MacBook_Pro --> host__resolve__anon__11_
  default_host_Ivys_MacBook_Pro --> host__resolve__anon__12_
  default_host_Ivys_MacBook_Pro --> host__resolve__anon__13_
  default_host_Ivys_MacBook_Pro --> host__resolve__anon__14_
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
  fonts_host_Ivys_MacBook_Pro --> fonts___when__0
  fonts_host_Ivys_MacBook_Pro --> fonts___when__1
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
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  shell_host_Ivys_MacBook_Pro --> jujutsu
  shell_host_Ivys_MacBook_Pro --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> host__resolve_vpn_
  vpn --> vpn_secrets
  vpn_secrets --> agenix_rekey_host_Ivys_MacBook_Pro
  end
  subgraph ctx_user_ivypierlot["user: ivypierlot"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__1_["<policy:hm-user-detect>[1]"]:::_policy_hm_user_detect__1__c
  _policy_main_ssh_key__to_hosts__5_["<policy:main-ssh-key/to-hosts>[5]"]:::_policy_main_ssh_key__to_hosts__5__c
  agenix_rekey_user_ivypierlot["agenix-rekey"]:::agenix_rekey_user_ivypierlot_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  packages__cotabby[/"packages/cotabby"\]:::packages__cotabby_c
  default_user_ivypierlot["default"]:::default_user_ivypierlot_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_nix["dev-nix"]:::dev_nix_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  file_local["file-local"]:::file_local_c
  fish["fish"]:::fish_c
  fonts_user_ivypierlot["fonts"]:::fonts_user_ivypierlot_c
  packages__ghostty[/"packages/ghostty"\]:::packages__ghostty_c
  ghostty["ghostty"]:::ghostty_c
  gpg["gpg"]:::gpg_c
  gui["gui"]:::gui_c
  packages__helium[/"packages/helium"\]:::packages__helium_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivypierlot{{"ivypierlot"}}:::ivypierlot_c
  ivypierlot___anon__8__to_hosts["ivypierlot/<anon>:8/to-hosts"]:::ivypierlot___anon__8__to_hosts_c
  ivypierlot__Ivys_MacBook_Pro["ivypierlot/Ivys-MacBook-Pro"]:::ivypierlot__Ivys_MacBook_Pro_c
  packages__jankyborders[/"packages/jankyborders"\]:::packages__jankyborders_c
  jankyborders["jankyborders"]:::jankyborders_c
  kanata["kanata"]:::kanata_c
  kind_system_routes_user_ivypierlot["kind-system-routes"]:::kind_system_routes_user_ivypierlot_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  neovim__to_users["neovim/to-users"]:::neovim__to_users_c
  nh_env["nh-env"]:::nh_env_c
  nixpkgs_config_user_ivypierlot["nixpkgs-config"]:::nixpkgs_config_user_ivypierlot_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nushell["nushell"]:::nushell_c
  onepassword["onepassword"]:::onepassword_c
  os_to_host_user_ivypierlot["os-to-host"]:::os_to_host_user_ivypierlot_c
  pam_rssh_user_ivypierlot["pam-rssh"]:::pam_rssh_user_ivypierlot_c
  pipe_unfree_user_ivypierlot["pipe-unfree"]:::pipe_unfree_user_ivypierlot_c
  den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro_{{"batteries/primary-user(ivypierlot@Ivys-MacBook-Pro)"}}:::den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro__c
  rift["rift"]:::rift_c
  shell_user_ivypierlot["shell"]:::shell_user_ivypierlot_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  packages__sketchybar[/"packages/sketchybar"\]:::packages__sketchybar_c
  sketchybar["sketchybar"]:::sketchybar_c
  packages__sketchybar_app_font[/"packages/sketchybar_app_font"\]:::packages__sketchybar_app_font_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_{{"provides/unfree(cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  user["user"]:::user_c
  user_shell__ivypierlot_Ivys_MacBook_Pro{{"user-shell/ivypierlot@Ivys-MacBook-Pro"}}:::user_shell__ivypierlot_Ivys_MacBook_Pro_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__0_["user/resolve(<anon>:0)"]:::user__resolve__anon__0__c
  user__resolve_dev_nix_{{"user/resolve(dev-nix)"}}:::user__resolve_dev_nix__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_ivypierlot_{{"user/resolve(ivypierlot)"}}:::user__resolve_ivypierlot__c
  user__resolve_jankyborders_{{"user/resolve(jankyborders)"}}:::user__resolve_jankyborders__c
  user__resolve_kanata_{{"user/resolve(kanata)"}}:::user__resolve_kanata__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  zotero["zotero"]:::zotero_c
  dev --> dev_cli
  dev --> dev_nix
  dev_nix --> user__resolve_dev_nix_
  fish --> shell_user_ivypierlot
  fish --> user_shell__ivypierlot_Ivys_MacBook_Pro
  fish --> user__resolve__anon__0_
  fish --> user__resolve_fish_
  ghostty --> packages__ghostty
  gui --> onepassword
  ivy_fetch --> packages__ivy_fetch
  ivypierlot --> agenix_rekey_user_ivypierlot
  ivypierlot --> packages__cotabby
  ivypierlot --> dev
  ivypierlot --> file_local
  ivypierlot --> fish
  ivypierlot --> ghostty
  ivypierlot --> gpg
  ivypierlot --> gui
  ivypierlot --> packages__helium
  ivypierlot --> kanata
  ivypierlot --> neovim
  ivypierlot --> nixvim
  ivypierlot --> nushell
  ivypierlot --> den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro_
  ivypierlot --> rift
  ivypierlot --> sketchybar
  ivypierlot --> user__resolve_ivypierlot_
  ivypierlot --> zotero
  jankyborders --> packages__jankyborders
  jankyborders --> user__resolve_jankyborders_
  kanata --> user__resolve_kanata_
  main_ssh_key --> pam_rssh_user_ivypierlot
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_
  onepassword --> den__provides__unfree_onepassword_password_manager_
  rift --> jankyborders
  sketchybar --> packages__sketchybar
  sketchybar --> packages__sketchybar_app_font
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__1_
  user --> _policy_main_ssh_key__to_hosts__5_
  user --> ivypierlot__Ivys_MacBook_Pro
  user --> default_user_ivypierlot
  user --> fonts_user_ivypierlot
  user --> ivy_fetch
  user --> ivypierlot
  user --> main_ssh_key
  user --> nixpkgs_config_user_ivypierlot
  user --> neovim__to_users
  user --> shell__to_users
  user --> user__resolve_user_
  end

  ivypierlot -.->|provides| ivypierlot__Ivys_MacBook_Pro__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__5__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef builders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__cotabby_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef file_local_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef packages__ghostty_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef packages__helium_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef homebrew_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivypierlot___anon__8__to_hosts_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivypierlot__Ivys_MacBook_Pro_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__jankyborders_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef jankyborders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef kanata_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef karabiner_driver_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_Ivys_MacBook_Pro_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_ivypierlot_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef rift_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__sketchybar_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef sketchybar_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__sketchybar_app_font_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sudoagents_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivypierlot__Ivys_MacBook_Pro__to_users_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivypierlot_Ivys_MacBook_Pro_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__0__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_dev_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_ivypierlot__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_jankyborders__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_kanata__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef zotero_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
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
  _policy_auscyber__auspc___anon__2__to_hosts__0_["<policy:auscyber/auspc/<anon>:2/to-hosts>[0]"]:::_policy_auscyber__auspc___anon__2__to_hosts__0__c
  agenix_rekey_host_auspc["agenix-rekey"]:::agenix_rekey_host_auspc_c
  bootlogo["bootlogo"]:::bootlogo_c
  builder_server["builder-server"]:::builder_server_c
  builders["builders"]:::builders_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_auspc["default"]:::default_host_auspc_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__auscyber_auspc{{"batteries/define-user/auscyber@auspc"}}:::den__batteries__define_user__auscyber_auspc_c
  difftastic["difftastic"]:::difftastic_c
  fonts_host_auspc["fonts"]:::fonts_host_auspc_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_auspc["nixpkgs-config"]:::nixpkgs_config_host_auspc_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_auspc["os-to-host"]:::os_to_host_host_auspc_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_auspc["pam-rssh"]:::pam_rssh_host_auspc_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_auspc["pipe-unfree"]:::pipe_unfree_host_auspc_c
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
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  auspc --> bootlogo
  auspc --> builder_server
  auspc --> builders
  auspc --> vpn
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_auspc --> den__batteries__define_user
  default_host_auspc --> home_base
  default_host_auspc --> host__resolve__anon__11_
  default_host_auspc --> host__resolve__anon__12_
  default_host_auspc --> host__resolve__anon__13_
  default_host_auspc --> host__resolve__anon__14_
  default_host_auspc -.-x host__resolve_default_
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
  fonts_host_auspc --> fonts___when__0
  fonts_host_auspc --> fonts___when__1
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
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  shell_host_auspc --> jujutsu
  shell_host_auspc --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> host__resolve_vpn_
  vpn --> vpn_secrets
  vpn_secrets --> agenix_rekey_host_auspc
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__2_["<policy:hm-user-detect>[2]"]:::_policy_hm_user_detect__2__c
  _policy_main_ssh_key__to_hosts__4_["<policy:main-ssh-key/to-hosts>[4]"]:::_policy_main_ssh_key__to_hosts__4__c
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber__auspc["auscyber/auspc"]:::auscyber__auspc_c
  default_user_auscyber["default"]:::default_user_auscyber_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_nix["dev-nix"]:::dev_nix_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  fish["fish"]:::fish_c
  fonts_user_auscyber["fonts"]:::fonts_user_auscyber_c
  packages__ghostty[/"packages/ghostty"\]:::packages__ghostty_c
  ghostty["ghostty"]:::ghostty_c
  gpg["gpg"]:::gpg_c
  gui["gui"]:::gui_c
  packages__helium[/"packages/helium"\]:::packages__helium_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  kind_system_routes_user_auscyber["kind-system-routes"]:::kind_system_routes_user_auscyber_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nixpkgs_config_user_auscyber["nixpkgs-config"]:::nixpkgs_config_user_auscyber_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nushell["nushell"]:::nushell_c
  onepassword["onepassword"]:::onepassword_c
  os_to_host_user_auscyber["os-to-host"]:::os_to_host_user_auscyber_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  pipe_unfree_user_auscyber["pipe-unfree"]:::pipe_unfree_user_auscyber_c
  den__batteries__primary_user_auscyber_auspc_{{"batteries/primary-user(auscyber@auspc)"}}:::den__batteries__primary_user_auscyber_auspc__c
  shell_user_auscyber["shell"]:::shell_user_auscyber_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_{{"provides/unfree(cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  user["user"]:::user_c
  user_shell__auscyber_auspc{{"user-shell/auscyber@auspc"}}:::user_shell__auscyber_auspc_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__0_["user/resolve(<anon>:0)"]:::user__resolve__anon__0__c
  auscyber__user__resolve_auspc__auscyber{{"auscyber/user/resolve(auspc):auscyber"}}:::auscyber__user__resolve_auspc__auscyber_c
  user__resolve_dev_nix_{{"user/resolve(dev-nix)"}}:::user__resolve_dev_nix__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  zotero["zotero"]:::zotero_c
  auscyber --> agenix_rekey_user_auscyber
  auscyber --> fish
  auscyber__auspc --> dev
  auscyber__auspc --> gpg
  auscyber__auspc --> gui
  auscyber__auspc --> packages__helium
  auscyber__auspc --> neovim
  auscyber__auspc --> nushell
  auscyber__auspc --> den__batteries__primary_user_auscyber_auspc_
  auscyber__auspc --> stylix
  auscyber__auspc --> auscyber__user__resolve_auspc__auscyber
  auscyber__auspc --> zotero
  dev --> dev_cli
  dev --> dev_nix
  dev_nix --> user__resolve_dev_nix_
  fish --> shell_user_auscyber
  fish --> user_shell__auscyber_auspc
  fish --> user__resolve__anon__0_
  fish --> user__resolve_fish_
  ghostty --> packages__ghostty
  gui --> ghostty
  gui --> onepassword
  ivy_fetch --> packages__ivy_fetch
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  nixvim --> packages__eagle_nvim
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_
  onepassword --> den__provides__unfree_onepassword_password_manager_
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__2_
  user --> _policy_main_ssh_key__to_hosts__4_
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
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_auscyber__auspc___anon__2__to_hosts__0__c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-width:2px
  classDef _policy_hm_user_detect__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__4__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef agenix_rekey_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber__auspc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef bootlogo_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef builder_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef builders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__auscyber_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef packages__ghostty_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef packages__helium_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef kind_system_routes_host_auspc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_auscyber_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_auscyber_auspc__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef auscyber__auspc__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__auscyber_auspc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__0__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef auscyber__user__resolve_auspc__auscyber_c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_dev_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef zotero_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
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
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_contabo["default"]:::default_host_contabo_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivy_contabo{{"batteries/define-user/ivy@contabo"}}:::den__batteries__define_user__ivy_contabo_c
  difftastic["difftastic"]:::difftastic_c
  fonts_host_contabo["fonts"]:::fonts_host_contabo_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_contabo["nixpkgs-config"]:::nixpkgs_config_host_contabo_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_contabo["os-to-host"]:::os_to_host_host_contabo_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_contabo["pam-rssh"]:::pam_rssh_host_contabo_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_contabo["pipe-unfree"]:::pipe_unfree_host_contabo_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell["shell"]:::shell_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_contabo --> agenix_rekey
  default_host_contabo --> den__batteries__define_user
  default_host_contabo --> home_base
  default_host_contabo --> host__resolve__anon__11_
  default_host_contabo --> host__resolve__anon__12_
  default_host_contabo --> host__resolve__anon__13_
  default_host_contabo --> host__resolve__anon__14_
  default_host_contabo -.-x host__resolve_default_
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
  fonts_host_contabo --> fonts___when__0
  fonts_host_contabo --> fonts___when__1
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
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  shell --> jujutsu
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivy["user: ivy"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__1_["<policy:hm-user-detect>[1]"]:::_policy_hm_user_detect__1__c
  _policy_main_ssh_key__to_hosts__4_["<policy:main-ssh-key/to-hosts>[4]"]:::_policy_main_ssh_key__to_hosts__4__c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_ivy["default"]:::default_user_ivy_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  fish["fish"]:::fish_c
  fonts_user_ivy["fonts"]:::fonts_user_ivy_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  ivy{{"ivy"}}:::ivy_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy__contabo["ivy/contabo"]:::ivy__contabo_c
  kind_system_routes_user_ivy["kind-system-routes"]:::kind_system_routes_user_ivy_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nixpkgs_config_user_ivy["nixpkgs-config"]:::nixpkgs_config_user_ivy_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  os_to_host_user_ivy["os-to-host"]:::os_to_host_user_ivy_c
  pam_rssh_user_ivy["pam-rssh"]:::pam_rssh_user_ivy_c
  pipe_unfree_user_ivy["pipe-unfree"]:::pipe_unfree_user_ivy_c
  den__batteries__primary_user_ivy_contabo_{{"batteries/primary-user(ivy@contabo)"}}:::den__batteries__primary_user_ivy_contabo__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_{{"provides/unfree(cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol__c
  user["user"]:::user_c
  user_shell__ivy_contabo{{"user-shell/ivy@contabo"}}:::user_shell__ivy_contabo_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__0_["user/resolve(<anon>:0)"]:::user__resolve__anon__0__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  fish --> user_shell__ivy_contabo
  fish --> user__resolve__anon__0_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  ivy__contabo --> fish
  ivy__contabo --> neovim
  ivy__contabo --> den__batteries__primary_user_ivy_contabo_
  main_ssh_key --> pam_rssh_user_ivy
  neovim --> nixvim
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__1_
  user --> _policy_main_ssh_key__to_hosts__4_
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
  classDef _policy_hm_user_detect__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__4__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivy_contabo_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_contabo_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_contabo_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivy_contabo__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
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
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivy_contabo_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__0__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_imflopet["default"]:::default_host_imflopet_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivy_imflopet{{"batteries/define-user/ivy@imflopet"}}:::den__batteries__define_user__ivy_imflopet_c
  difftastic["difftastic"]:::difftastic_c
  fonts_host_imflopet["fonts"]:::fonts_host_imflopet_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_imflopet["nixpkgs-config"]:::nixpkgs_config_host_imflopet_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_imflopet["os-to-host"]:::os_to_host_host_imflopet_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_imflopet["pam-rssh"]:::pam_rssh_host_imflopet_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_imflopet["pipe-unfree"]:::pipe_unfree_host_imflopet_c
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
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_imflopet --> agenix_rekey
  default_host_imflopet --> den__batteries__define_user
  default_host_imflopet --> home_base
  default_host_imflopet --> host__resolve__anon__11_
  default_host_imflopet --> host__resolve__anon__12_
  default_host_imflopet --> host__resolve__anon__13_
  default_host_imflopet --> host__resolve__anon__14_
  default_host_imflopet -.-x host__resolve_default_
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
  fonts_host_imflopet --> fonts___when__0
  fonts_host_imflopet --> fonts___when__1
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
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  shell --> jujutsu
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivy["user: ivy"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__1_["<policy:hm-user-detect>[1]"]:::_policy_hm_user_detect__1__c
  _policy_main_ssh_key__to_hosts__4_["<policy:main-ssh-key/to-hosts>[4]"]:::_policy_main_ssh_key__to_hosts__4__c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_ivy["default"]:::default_user_ivy_c
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
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nixpkgs_config_user_ivy["nixpkgs-config"]:::nixpkgs_config_user_ivy_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  os_to_host_user_ivy["os-to-host"]:::os_to_host_user_ivy_c
  pam_rssh_user_ivy["pam-rssh"]:::pam_rssh_user_ivy_c
  pipe_unfree_user_ivy["pipe-unfree"]:::pipe_unfree_user_ivy_c
  den__batteries__primary_user_ivy_imflopet_{{"batteries/primary-user(ivy@imflopet)"}}:::den__batteries__primary_user_ivy_imflopet__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_{{"provides/unfree(cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol__c
  user["user"]:::user_c
  user_shell__ivy_imflopet{{"user-shell/ivy@imflopet"}}:::user_shell__ivy_imflopet_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__0_["user/resolve(<anon>:0)"]:::user__resolve__anon__0__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  fish --> user_shell__ivy_imflopet
  fish --> user__resolve__anon__0_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  ivy__imflopet --> fish
  ivy__imflopet --> gpg
  ivy__imflopet --> neovim
  ivy__imflopet --> den__batteries__primary_user_ivy_imflopet_
  main_ssh_key --> pam_rssh_user_ivy
  neovim --> nixvim
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__1_
  user --> _policy_main_ssh_key__to_hosts__4_
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
  classDef _policy_hm_user_detect__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__4__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivy_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivy_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_imflopet_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivy_imflopet__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
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
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivy_imflopet_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__0__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
style ctx_host_imflopet fill:#d0d7de,stroke:#8c959f,stroke-width:2px
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
  agenix_rekey_host_macmini["agenix-rekey"]:::agenix_rekey_host_macmini_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_macmini["default"]:::default_host_macmini_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivypierlot_macmini{{"batteries/define-user/ivypierlot@macmini"}}:::den__batteries__define_user__ivypierlot_macmini_c
  difftastic["difftastic"]:::difftastic_c
  fonts_host_macmini["fonts"]:::fonts_host_macmini_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  homebrew["homebrew"]:::homebrew_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_macmini["nixpkgs-config"]:::nixpkgs_config_host_macmini_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_macmini["os-to-host"]:::os_to_host_host_macmini_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_macmini["pam-rssh"]:::pam_rssh_host_macmini_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_macmini["pipe-unfree"]:::pipe_unfree_host_macmini_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_macmini["shell"]:::shell_host_macmini_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_macmini --> agenix_rekey_host_macmini
  default_host_macmini --> den__batteries__define_user
  default_host_macmini --> home_base
  default_host_macmini --> host__resolve__anon__11_
  default_host_macmini --> host__resolve__anon__12_
  default_host_macmini --> host__resolve__anon__13_
  default_host_macmini --> host__resolve__anon__14_
  default_host_macmini -.-x host__resolve_default_
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
  fonts_host_macmini --> fonts___when__0
  fonts_host_macmini --> fonts___when__1
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
  macmini --> homebrew
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  shell_host_macmini --> jujutsu
  shell_host_macmini --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivypierlot["user: ivypierlot"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__1_["<policy:hm-user-detect>[1]"]:::_policy_hm_user_detect__1__c
  _policy_main_ssh_key__to_hosts__5_["<policy:main-ssh-key/to-hosts>[5]"]:::_policy_main_ssh_key__to_hosts__5__c
  agenix_rekey_user_ivypierlot["agenix-rekey"]:::agenix_rekey_user_ivypierlot_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  packages__cotabby[/"packages/cotabby"\]:::packages__cotabby_c
  default_user_ivypierlot["default"]:::default_user_ivypierlot_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_nix["dev-nix"]:::dev_nix_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  file_local["file-local"]:::file_local_c
  fish["fish"]:::fish_c
  fonts_user_ivypierlot["fonts"]:::fonts_user_ivypierlot_c
  packages__ghostty[/"packages/ghostty"\]:::packages__ghostty_c
  ghostty["ghostty"]:::ghostty_c
  gpg["gpg"]:::gpg_c
  gui["gui"]:::gui_c
  packages__helium[/"packages/helium"\]:::packages__helium_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivypierlot{{"ivypierlot"}}:::ivypierlot_c
  ivypierlot___anon__8__to_hosts["ivypierlot/<anon>:8/to-hosts"]:::ivypierlot___anon__8__to_hosts_c
  ivypierlot__macmini["ivypierlot/macmini"]:::ivypierlot__macmini_c
  packages__jankyborders[/"packages/jankyborders"\]:::packages__jankyborders_c
  jankyborders["jankyborders"]:::jankyborders_c
  kanata["kanata"]:::kanata_c
  kind_system_routes_user_ivypierlot["kind-system-routes"]:::kind_system_routes_user_ivypierlot_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  neovim__to_users["neovim/to-users"]:::neovim__to_users_c
  nh_env["nh-env"]:::nh_env_c
  nixpkgs_config_user_ivypierlot["nixpkgs-config"]:::nixpkgs_config_user_ivypierlot_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  nushell["nushell"]:::nushell_c
  onepassword["onepassword"]:::onepassword_c
  os_to_host_user_ivypierlot["os-to-host"]:::os_to_host_user_ivypierlot_c
  pam_rssh_user_ivypierlot["pam-rssh"]:::pam_rssh_user_ivypierlot_c
  pipe_unfree_user_ivypierlot["pipe-unfree"]:::pipe_unfree_user_ivypierlot_c
  den__batteries__primary_user_ivypierlot_macmini_{{"batteries/primary-user(ivypierlot@macmini)"}}:::den__batteries__primary_user_ivypierlot_macmini__c
  rift["rift"]:::rift_c
  shell_user_ivypierlot["shell"]:::shell_user_ivypierlot_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  packages__sketchybar[/"packages/sketchybar"\]:::packages__sketchybar_c
  sketchybar["sketchybar"]:::sketchybar_c
  packages__sketchybar_app_font[/"packages/sketchybar_app_font"\]:::packages__sketchybar_app_font_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_{{"provides/unfree(cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  user["user"]:::user_c
  user_shell__ivypierlot_macmini{{"user-shell/ivypierlot@macmini"}}:::user_shell__ivypierlot_macmini_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__0_["user/resolve(<anon>:0)"]:::user__resolve__anon__0__c
  user__resolve_dev_nix_{{"user/resolve(dev-nix)"}}:::user__resolve_dev_nix__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_ivypierlot_{{"user/resolve(ivypierlot)"}}:::user__resolve_ivypierlot__c
  user__resolve_jankyborders_{{"user/resolve(jankyborders)"}}:::user__resolve_jankyborders__c
  user__resolve_kanata_{{"user/resolve(kanata)"}}:::user__resolve_kanata__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  zotero["zotero"]:::zotero_c
  dev --> dev_cli
  dev --> dev_nix
  dev_nix --> user__resolve_dev_nix_
  fish --> shell_user_ivypierlot
  fish --> user_shell__ivypierlot_macmini
  fish --> user__resolve__anon__0_
  fish --> user__resolve_fish_
  ghostty --> packages__ghostty
  gui --> onepassword
  ivy_fetch --> packages__ivy_fetch
  ivypierlot --> agenix_rekey_user_ivypierlot
  ivypierlot --> packages__cotabby
  ivypierlot --> dev
  ivypierlot --> file_local
  ivypierlot --> fish
  ivypierlot --> ghostty
  ivypierlot --> gpg
  ivypierlot --> gui
  ivypierlot --> packages__helium
  ivypierlot --> kanata
  ivypierlot --> neovim
  ivypierlot --> nixvim
  ivypierlot --> nushell
  ivypierlot --> den__batteries__primary_user_ivypierlot_macmini_
  ivypierlot --> rift
  ivypierlot --> sketchybar
  ivypierlot --> user__resolve_ivypierlot_
  ivypierlot --> zotero
  jankyborders --> packages__jankyborders
  jankyborders --> user__resolve_jankyborders_
  kanata --> user__resolve_kanata_
  main_ssh_key --> pam_rssh_user_ivypierlot
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_
  onepassword --> den__provides__unfree_onepassword_password_manager_
  rift --> jankyborders
  sketchybar --> packages__sketchybar
  sketchybar --> packages__sketchybar_app_font
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__1_
  user --> _policy_main_ssh_key__to_hosts__5_
  user --> default_user_ivypierlot
  user --> fonts_user_ivypierlot
  user --> ivy_fetch
  user --> ivypierlot
  user --> ivypierlot__macmini
  user --> main_ssh_key
  user --> nixpkgs_config_user_ivypierlot
  user --> neovim__to_users
  user --> shell__to_users
  user --> user__resolve_user_
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__5__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef agenix_rekey_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__cotabby_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivypierlot_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef file_local_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef packages__ghostty_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef packages__helium_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef homebrew_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivypierlot___anon__8__to_hosts_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef ivypierlot__macmini_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__jankyborders_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef jankyborders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef kanata_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef kind_system_routes_host_macmini_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_ivypierlot_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_ivypierlot_macmini__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef rift_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef packages__sketchybar_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef sketchybar_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__sketchybar_app_font_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__ivypierlot_macmini_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__0__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_dev_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_ivypierlot__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_jankyborders__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_kanata__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef zotero_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
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
  fonts_host_pentestvm["fonts"]:::fonts_host_pentestvm_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_pentestvm["nixpkgs-config"]:::nixpkgs_config_host_pentestvm_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_pentestvm["os-to-host"]:::os_to_host_host_pentestvm_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_pentestvm["pam-rssh"]:::pam_rssh_host_pentestvm_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_pentestvm["pipe-unfree"]:::pipe_unfree_host_pentestvm_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell["shell"]:::shell_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_pentestvm --> agenix_rekey
  default_host_pentestvm --> den__batteries__define_user
  default_host_pentestvm --> home_base
  default_host_pentestvm --> host__resolve__anon__11_
  default_host_pentestvm --> host__resolve__anon__12_
  default_host_pentestvm --> host__resolve__anon__13_
  default_host_pentestvm --> host__resolve__anon__14_
  default_host_pentestvm -.-x host__resolve_default_
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
  fonts_host_pentestvm --> fonts___when__0
  fonts_host_pentestvm --> fonts___when__1
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
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  shell --> jujutsu
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_admin["user: admin"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__1_["<policy:hm-user-detect>[1]"]:::_policy_hm_user_detect__1__c
  _policy_main_ssh_key__to_hosts__3_["<policy:main-ssh-key/to-hosts>[3]"]:::_policy_main_ssh_key__to_hosts__3__c
  admin{{"admin"}}:::admin_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_admin["default"]:::default_user_admin_c
  fonts_user_admin["fonts"]:::fonts_user_admin_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  kind_system_routes_user_admin["kind-system-routes"]:::kind_system_routes_user_admin_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  nh_env["nh-env"]:::nh_env_c
  nixpkgs_config_user_admin["nixpkgs-config"]:::nixpkgs_config_user_admin_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  os_to_host_user_admin["os-to-host"]:::os_to_host_user_admin_c
  pam_rssh_user_admin["pam-rssh"]:::pam_rssh_user_admin_c
  pipe_unfree_user_admin["pipe-unfree"]:::pipe_unfree_user_admin_c
  den__batteries__primary_user_admin_pentestvm_{{"batteries/primary-user(admin@pentestvm)"}}:::den__batteries__primary_user_admin_pentestvm__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  user["user"]:::user_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  admin --> den__batteries__primary_user_admin_pentestvm_
  ivy_fetch --> packages__ivy_fetch
  main_ssh_key --> pam_rssh_user_admin
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__1_
  user --> _policy_main_ssh_key__to_hosts__3_
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
  classDef _policy_hm_user_detect__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__3__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef admin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__admin_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_pentestvm_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_admin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pentestvm_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_admin_pentestvm__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
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
  fonts_host_secondpc["fonts"]:::fonts_host_secondpc_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nginx_{{"host/resolve(nginx)"}}:::host__resolve_nginx__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nginx["nginx"]:::nginx_c
  nix["nix"]:::nix_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_secondpc["nixpkgs-config"]:::nixpkgs_config_host_secondpc_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_secondpc["os-to-host"]:::os_to_host_host_secondpc_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_secondpc["pam-rssh"]:::pam_rssh_host_secondpc_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_secondpc["pipe-unfree"]:::pipe_unfree_host_secondpc_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_secondpc["shell"]:::shell_host_secondpc_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  vpn_server["vpn-server"]:::vpn_server_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_secondpc --> den__batteries__define_user
  default_host_secondpc --> home_base
  default_host_secondpc --> host__resolve__anon__11_
  default_host_secondpc --> host__resolve__anon__12_
  default_host_secondpc --> host__resolve__anon__13_
  default_host_secondpc --> host__resolve__anon__14_
  default_host_secondpc -.-x host__resolve_default_
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
  fonts_host_secondpc --> fonts___when__0
  fonts_host_secondpc --> fonts___when__1
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
  nginx --> host__resolve_nginx_
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  secondpc --> builder_server
  secondpc --> builders
  secondpc --> local
  secondpc --> nginx
  secondpc --> nix
  secondpc --> vpn_server
  shell_host_secondpc --> jujutsu
  shell_host_secondpc --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> host__resolve_vpn_
  vpn --> vpn_secrets
  vpn_secrets --> agenix_rekey_host_secondpc
  vpn_server --> vpn
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__2_["<policy:hm-user-detect>[2]"]:::_policy_hm_user_detect__2__c
  _policy_main_ssh_key__to_hosts__4_["<policy:main-ssh-key/to-hosts>[4]"]:::_policy_main_ssh_key__to_hosts__4__c
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber__secondpc["auscyber/secondpc"]:::auscyber__secondpc_c
  default_user_auscyber["default"]:::default_user_auscyber_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  fish["fish"]:::fish_c
  fonts_user_auscyber["fonts"]:::fonts_user_auscyber_c
  gpg["gpg"]:::gpg_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  kind_system_routes_user_auscyber["kind-system-routes"]:::kind_system_routes_user_auscyber_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nixpkgs_config_user_auscyber["nixpkgs-config"]:::nixpkgs_config_user_auscyber_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  os_to_host_user_auscyber["os-to-host"]:::os_to_host_user_auscyber_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  pipe_unfree_user_auscyber["pipe-unfree"]:::pipe_unfree_user_auscyber_c
  den__batteries__primary_user_auscyber_secondpc_{{"batteries/primary-user(auscyber@secondpc)"}}:::den__batteries__primary_user_auscyber_secondpc__c
  shell_user_auscyber["shell"]:::shell_user_auscyber_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_{{"provides/unfree(cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol__c
  user["user"]:::user_c
  user_shell__auscyber_secondpc{{"user-shell/auscyber@secondpc"}}:::user_shell__auscyber_secondpc_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__0_["user/resolve(<anon>:0)"]:::user__resolve__anon__0__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  auscyber --> agenix_rekey_user_auscyber
  auscyber --> fish
  auscyber__secondpc --> gpg
  auscyber__secondpc --> neovim
  auscyber__secondpc --> den__batteries__primary_user_auscyber_secondpc_
  fish --> shell_user_auscyber
  fish --> user_shell__auscyber_secondpc
  fish --> user__resolve__anon__0_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__2_
  user --> _policy_main_ssh_key__to_hosts__4_
  user --> auscyber
  user --> default_user_auscyber
  user --> fonts_user_auscyber
  user --> ivy_fetch
  user --> main_ssh_key
  user --> nixpkgs_config_user_auscyber
  user --> auscyber__secondpc
  user --> shell__to_users
  user --> user__resolve_user_
  end

  neovim -.->|provides| neovim__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef _anon__c fill:#e16f24,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__4__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef agenix_rekey_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber__secondpc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef builder_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef builders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__auscyber_secondpc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nginx__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef local_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nginx_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_auscyber_secondpc__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef secondpc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
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
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__auscyber_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__0__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef vpn_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
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
  agenix_rekey_host_surfacelaptop["agenix-rekey"]:::agenix_rekey_host_surfacelaptop_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  default_host_surfacelaptop["default"]:::default_host_surfacelaptop_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__auscyber_surfacelaptop{{"batteries/define-user/auscyber@surfacelaptop"}}:::den__batteries__define_user__auscyber_surfacelaptop_c
  difftastic["difftastic"]:::difftastic_c
  fonts_host_surfacelaptop["fonts"]:::fonts_host_surfacelaptop_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_surfacelaptop["nixpkgs-config"]:::nixpkgs_config_host_surfacelaptop_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_surfacelaptop["os-to-host"]:::os_to_host_host_surfacelaptop_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_surfacelaptop["pam-rssh"]:::pam_rssh_host_surfacelaptop_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_surfacelaptop["pipe-unfree"]:::pipe_unfree_host_surfacelaptop_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_surfacelaptop["shell"]:::shell_host_surfacelaptop_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  neovim__to_users[/"neovim/to-users"\]:::neovim__to_users_c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_surfacelaptop --> agenix_rekey_host_surfacelaptop
  default_host_surfacelaptop --> den__batteries__define_user
  default_host_surfacelaptop --> home_base
  default_host_surfacelaptop --> host__resolve__anon__11_
  default_host_surfacelaptop --> host__resolve__anon__12_
  default_host_surfacelaptop --> host__resolve__anon__13_
  default_host_surfacelaptop --> host__resolve__anon__14_
  default_host_surfacelaptop -.-x host__resolve_default_
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
  fonts_host_surfacelaptop --> fonts___when__0
  fonts_host_surfacelaptop --> fonts___when__1
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
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  shell_host_surfacelaptop --> jujutsu
  shell_host_surfacelaptop --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__2_["<policy:hm-user-detect>[2]"]:::_policy_hm_user_detect__2__c
  _policy_main_ssh_key__to_hosts__4_["<policy:main-ssh-key/to-hosts>[4]"]:::_policy_main_ssh_key__to_hosts__4__c
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber__surfacelaptop["auscyber/surfacelaptop"]:::auscyber__surfacelaptop_c
  default_user_auscyber["default"]:::default_user_auscyber_c
  packages__eagle_nvim[/"packages/eagle-nvim"\]:::packages__eagle_nvim_c
  fish["fish"]:::fish_c
  fonts_user_auscyber["fonts"]:::fonts_user_auscyber_c
  gpg["gpg"]:::gpg_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  kind_system_routes_user_auscyber["kind-system-routes"]:::kind_system_routes_user_auscyber_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  neovim["neovim"]:::neovim_c
  nh_env["nh-env"]:::nh_env_c
  nixpkgs_config_user_auscyber["nixpkgs-config"]:::nixpkgs_config_user_auscyber_c
  nixvim["nixvim"]:::nixvim_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  os_to_host_user_auscyber["os-to-host"]:::os_to_host_user_auscyber_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  pipe_unfree_user_auscyber["pipe-unfree"]:::pipe_unfree_user_auscyber_c
  den__batteries__primary_user_auscyber_surfacelaptop_{{"batteries/primary-user(auscyber@surfacelaptop)"}}:::den__batteries__primary_user_auscyber_surfacelaptop__c
  shell_user_auscyber["shell"]:::shell_user_auscyber_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_{{"provides/unfree(cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol__c
  user["user"]:::user_c
  user_shell__auscyber_surfacelaptop{{"user-shell/auscyber@surfacelaptop"}}:::user_shell__auscyber_surfacelaptop_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__0_["user/resolve(<anon>:0)"]:::user__resolve__anon__0__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  auscyber --> agenix_rekey_user_auscyber
  auscyber --> fish
  auscyber__surfacelaptop --> gpg
  auscyber__surfacelaptop --> neovim
  auscyber__surfacelaptop --> den__batteries__primary_user_auscyber_surfacelaptop_
  fish --> shell_user_auscyber
  fish --> user_shell__auscyber_surfacelaptop
  fish --> user__resolve__anon__0_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  neovim --> stylix
  nixvim --> packages__eagle_nvim
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__2_
  user --> _policy_main_ssh_key__to_hosts__4_
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
  classDef _policy_agenix_rekey__to_users__0__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_hm_user_detect__2__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__4__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef agenix_rekey_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber__surfacelaptop_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__auscyber_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef packages__eagle_nvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef kind_system_routes_host_surfacelaptop_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef kind_system_routes_user_auscyber_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef main_ssh_key__to_hosts_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nh_env_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_auscyber_surfacelaptop__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__batteries__self___user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef shell_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__sources__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__auscyber_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__0__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  fonts_host_wsl_nixos["fonts"]:::fonts_host_wsl_nixos_c
  fonts___when__0["fonts/<when>:0"]:::fonts___when__0_c
  fonts___when__1["fonts/<when>:1"]:::fonts___when__1_c
  home_base["home-base"]:::home_base_c
  host["host"]:::host_c
  host_to_hm_users["host-to-hm-users"]:::host_to_hm_users_c
  host_to_users["host-to-users"]:::host_to_users_c
  host__resolve__anon__11_["host/resolve(<anon>:11)"]:::host__resolve__anon__11__c
  host__resolve__anon__12_["host/resolve(<anon>:12)"]:::host__resolve__anon__12__c
  host__resolve__anon__13_["host/resolve(<anon>:13)"]:::host__resolve__anon__13__c
  host__resolve__anon__14_["host/resolve(<anon>:14)"]:::host__resolve__anon__14__c
  host__resolve_default_["host/resolve(default)"]:::host__resolve_default__c
  host__resolve_difftastic_{{"host/resolve(difftastic)"}}:::host__resolve_difftastic__c
  host__resolve_host_["host/resolve(host)"]:::host__resolve_host__c
  host__resolve_nix_{{"host/resolve(nix)"}}:::host__resolve_nix__c
  host__resolve_overlays_{{"host/resolve(overlays)"}}:::host__resolve_overlays__c
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
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_wsl_nixos["nixpkgs-config"]:::nixpkgs_config_host_wsl_nixos_c
  openssh["openssh"]:::openssh_c
  os_to_host_host_wsl_nixos["os-to-host"]:::os_to_host_host_wsl_nixos_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_wsl_nixos["pam-rssh"]:::pam_rssh_host_wsl_nixos_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  pipe_unfree_host_wsl_nixos["pipe-unfree"]:::pipe_unfree_host_wsl_nixos_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  den__batteries__self___user{{"batteries/self'/user"}}:::den__batteries__self___user_c
  shell_host_wsl_nixos["shell"]:::shell_host_wsl_nixos_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  den__batteries__sources__user{{"batteries/sources/user"}}:::den__batteries__sources__user_c
  starship["starship"]:::starship_c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  default_host_wsl_nixos --> agenix_rekey
  default_host_wsl_nixos --> den__batteries__define_user
  default_host_wsl_nixos --> home_base
  default_host_wsl_nixos --> host__resolve__anon__11_
  default_host_wsl_nixos --> host__resolve__anon__12_
  default_host_wsl_nixos --> host__resolve__anon__13_
  default_host_wsl_nixos --> host__resolve__anon__14_
  default_host_wsl_nixos -.-x host__resolve_default_
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
  fonts_host_wsl_nixos --> fonts___when__0
  fonts_host_wsl_nixos --> fonts___when__1
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
  nix --> host__resolve_nix_
  overlays --> host__resolve_overlays_
  shell_host_wsl_nixos --> jujutsu
  shell_host_wsl_nixos --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_nixos["user: nixos"]
  _policy_agenix_rekey__to_users__0_["<policy:agenix-rekey/to-users>[0]"]:::_policy_agenix_rekey__to_users__0__c
  _policy_hm_user_detect__1_["<policy:hm-user-detect>[1]"]:::_policy_hm_user_detect__1__c
  _policy_main_ssh_key__to_hosts__3_["<policy:main-ssh-key/to-hosts>[3]"]:::_policy_main_ssh_key__to_hosts__3__c
  agenix_rekey__to_users["agenix-rekey/to-users"]:::agenix_rekey__to_users_c
  default_user_nixos["default"]:::default_user_nixos_c
  fish["fish"]:::fish_c
  fonts_user_nixos["fonts"]:::fonts_user_nixos_c
  hm_user_detect["hm-user-detect"]:::hm_user_detect_c
  packages__ivy_fetch[/"packages/ivy-fetch"\]:::packages__ivy_fetch_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  kind_system_routes_user_nixos["kind-system-routes"]:::kind_system_routes_user_nixos_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  main_ssh_key__to_hosts["main-ssh-key/to-hosts"]:::main_ssh_key__to_hosts_c
  nh_env["nh-env"]:::nh_env_c
  nixos{{"nixos"}}:::nixos_c
  wsl_nixos__nixos[/"wsl-nixos/nixos"\]:::wsl_nixos__nixos_c
  nixos__to_users["nixos/to-users"]:::nixos__to_users_c
  nixpkgs_config_user_nixos["nixpkgs-config"]:::nixpkgs_config_user_nixos_c
  nixvim_hm_module["nixvim-hm-module"]:::nixvim_hm_module_c
  os_to_host_user_nixos["os-to-host"]:::os_to_host_user_nixos_c
  pam_rssh_user_nixos["pam-rssh"]:::pam_rssh_user_nixos_c
  pipe_unfree_user_nixos["pipe-unfree"]:::pipe_unfree_user_nixos_c
  den__batteries__primary_user_nixos_wsl_nixos_{{"batteries/primary-user(nixos@wsl-nixos)"}}:::den__batteries__primary_user_nixos_wsl_nixos__c
  shell_user_nixos["shell"]:::shell_user_nixos_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  user["user"]:::user_c
  user_shell__nixos_wsl_nixos{{"user-shell/nixos@wsl-nixos"}}:::user_shell__nixos_wsl_nixos_c
  user_to_host["user-to-host"]:::user_to_host_c
  user__resolve__anon__0_["user/resolve(<anon>:0)"]:::user__resolve__anon__0__c
  user__resolve_fish_{{"user/resolve(fish)"}}:::user__resolve_fish__c
  user__resolve_user_["user/resolve(user)"]:::user__resolve_user__c
  fish --> shell_user_nixos
  fish --> user_shell__nixos_wsl_nixos
  fish --> user__resolve__anon__0_
  fish --> user__resolve_fish_
  ivy_fetch --> packages__ivy_fetch
  main_ssh_key --> pam_rssh_user_nixos
  nixos --> fish
  nixos --> den__batteries__primary_user_nixos_wsl_nixos_
  user --> _policy_agenix_rekey__to_users__0_
  user --> _policy_hm_user_detect__1_
  user --> _policy_main_ssh_key__to_hosts__3_
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
  classDef _policy_hm_user_detect__1__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef _policy_main_ssh_key__to_hosts__3__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef default_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef default_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__nixos_wsl_nixos_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef difftastic_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef fonts_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fonts___when__0_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef fonts___when__1_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef hm_user_detect_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host_to_hm_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host_to_users_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef host__resolve__anon__11__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__12__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__13__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef host__resolve__anon__14__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef host__resolve_default__c fill:#a475f9,stroke:#fa4549,color:#1f2328,stroke-dasharray: 5 5,stroke-width:2px
  classDef host__resolve_difftastic__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_host__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_nix__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef host__resolve_overlays__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
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
  classDef nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef wsl_nixos__nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos__to_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixpkgs_config_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixvim_hm_module_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef os_to_host_host_wsl_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef os_to_host_user_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pipe_unfree_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef pipe_unfree_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__primary_user_nixos_wsl_nixos__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
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
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef user_shell__nixos_wsl_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef user_to_host_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef user__resolve__anon__0__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_fish__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef user__resolve_user__c fill:#d0d7de,stroke:#8c959f,color:#424a53,stroke-dasharray: 2 2,stroke-width:1px
  classDef wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
style ctx_host_wsl_nixos fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_nixos fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```


## Other Projects
- [xmonad](https://github.com/xmonad/xmonad)
- [ghostty](https://github.com/ghostty-org/ghostty)
- [neovim](https://github.com/neovim/neovim)
- [starship](https://github.com/starship/starship)
