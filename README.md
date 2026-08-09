# My Dotfiles
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

## Systems

| System | Architecture | Roles |
|--------|--------------|-------|
| Ivys-MacBook-Pro | aarch64-darwin | study, gui, dev |
| auspc | x86_64-linux | gui, gaming, dev |
| lora-pi | aarch64-linux |  |
| macmini | aarch64-darwin | gui |
| pentestvm | x86_64-linux |  |
| secondpc | x86_64-linux |  |
| surfacelaptop | x86_64-linux | gui, dev |
| wsl-nixos | x86_64-linux |  |

## Ivys-MacBook-Pro

**Architecture:** `aarch64-darwin`
**Roles:** study, gui, dev

### Aspects
- Ivys-MacBook-Pro/to-users
- agenix-rekey
- batteries/define-user
- batteries/define-user/ivypierlot@Ivys-MacBook-Pro
- batteries/hostname
- batteries/hostname/os
- batteries/inputs&#39;
- batteries/inputs&#39;/os
- batteries/primary-user(ivypierlot@Ivys-MacBook-Pro)
- batteries/self&#39;
- batteries/self&#39;/os
- batteries/sources
- batteries/sources/os
- browsers
- browsers/zen
- ccache
- ccache/Ivys-MacBook-Pro
- celler-push
- claude
- cotabby
- darwin-base
- darwin-finder
- darwin-general
- darwin-hmApps
- dev
- dev-cli
- dev-nix
- file-local
- fish
- ghostty
- gpg
- gui
- home-base
- homebrew
- idris
- insecure-predicate
- insecure-predicate/os
- insecure-predicate/user
- ivy-fetch
- ivypierlot
- jujutsu
- karabiner-driver
- lib
- llama-cpp
- main-ssh-key
- mcp-servers
- neovim
- nix
- nix-index
- nixpkgs-config
- nixvim
- nushell
- onepassword
- opencode
- overlays
- pam-rssh
- pam-touchid
- paneru
- provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)
- provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)
- provides/unfree(libkey-nomad,onepassword-password-manager)
- provides/unfree(onepassword-password-manager)
- shell
- shell/to-users
- sketchybar
- starship
- stylix
- sudoagents
- unfree-predicate
- unfree-predicate/os
- unfree-predicate/user
- user-shell/ivypierlot@Ivys-MacBook-Pro
- vpn
- vpn-secrets
- wakatime
- zotero

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  Ivys_MacBook_Pro([Ivys-MacBook-Pro]):::root

  subgraph ctx_host_Ivys_MacBook_Pro["host: Ivys-MacBook-Pro"]
  ccache__Ivys_MacBook_Pro[/"ccache/Ivys-MacBook-Pro"\]:::ccache__Ivys_MacBook_Pro_c
  agenix_rekey_host_Ivys_MacBook_Pro["agenix-rekey"]:::agenix_rekey_host_Ivys_MacBook_Pro_c
  ccache["ccache"]:::ccache_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro{{"batteries/define-user/ivypierlot@Ivys-MacBook-Pro"}}:::den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro_c
  home_base["home-base"]:::home_base_c
  homebrew_host_Ivys_MacBook_Pro["homebrew"]:::homebrew_host_Ivys_MacBook_Pro_c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  karabiner_driver["karabiner-driver"]:::karabiner_driver_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nixpkgs_config_host_Ivys_MacBook_Pro["nixpkgs-config"]:::nixpkgs_config_host_Ivys_MacBook_Pro_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_Ivys_MacBook_Pro["pam-rssh"]:::pam_rssh_host_Ivys_MacBook_Pro_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  shell_host_Ivys_MacBook_Pro["shell"]:::shell_host_Ivys_MacBook_Pro_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  starship["starship"]:::starship_c
  sudoagents_host_Ivys_MacBook_Pro["sudoagents"]:::sudoagents_host_Ivys_MacBook_Pro_c
  ivypierlot__Ivys_MacBook_Pro__to_users[/"Ivys-MacBook-Pro/to-users"\]:::ivypierlot__Ivys_MacBook_Pro__to_users_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  Ivys_MacBook_Pro --> ccache
  Ivys_MacBook_Pro --> homebrew_host_Ivys_MacBook_Pro
  Ivys_MacBook_Pro --> karabiner_driver
  Ivys_MacBook_Pro --> sudoagents_host_Ivys_MacBook_Pro
  Ivys_MacBook_Pro --> vpn
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  den__batteries__define_user --> den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__sources --> den__batteries__sources__os
  home_base --> shell_host_Ivys_MacBook_Pro
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  nixpkgs_config_host_Ivys_MacBook_Pro --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_Ivys_MacBook_Pro --> jujutsu
  shell_host_Ivys_MacBook_Pro --> nix_index
  shell_host_Ivys_MacBook_Pro --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> vpn_secrets
  vpn_secrets --> agenix_rekey_host_Ivys_MacBook_Pro
  ccache -.->|provides| ccache__Ivys_MacBook_Pro
  end
  subgraph ctx_user_ivypierlot["user: ivypierlot"]
  agenix_rekey_user_ivypierlot["agenix-rekey"]:::agenix_rekey_user_ivypierlot_c
  celler_push["celler-push"]:::celler_push_c
  claude["claude"]:::claude_c
  cotabby["cotabby"]:::cotabby_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_nix["dev-nix"]:::dev_nix_c
  file_local["file-local"]:::file_local_c
  fish["fish"]:::fish_c
  ghostty["ghostty"]:::ghostty_c
  gpg["gpg"]:::gpg_c
  gui["gui"]:::gui_c
  homebrew_user_ivypierlot["homebrew"]:::homebrew_user_ivypierlot_c
  idris["idris"]:::idris_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivypierlot{{"ivypierlot"}}:::ivypierlot_c
  llama_cpp["llama-cpp"]:::llama_cpp_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  mcp_servers["mcp-servers"]:::mcp_servers_c
  neovim["neovim"]:::neovim_c
  nixvim["nixvim"]:::nixvim_c
  nushell["nushell"]:::nushell_c
  onepassword["onepassword"]:::onepassword_c
  opencode["opencode"]:::opencode_c
  pam_rssh_user_ivypierlot["pam-rssh"]:::pam_rssh_user_ivypierlot_c
  paneru["paneru"]:::paneru_c
  den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro_{{"batteries/primary-user(ivypierlot@Ivys-MacBook-Pro)"}}:::den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  sketchybar["sketchybar"]:::sketchybar_c
  stylix["stylix"]:::stylix_c
  sudoagents_user_ivypierlot["sudoagents"]:::sudoagents_user_ivypierlot_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  den__provides__unfree_libkey_nomad_onepassword_password_manager_{{"provides/unfree(libkey-nomad,onepassword-password-manager)"}}:::den__provides__unfree_libkey_nomad_onepassword_password_manager__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  user_shell__ivypierlot_Ivys_MacBook_Pro{{"user-shell/ivypierlot@Ivys-MacBook-Pro"}}:::user_shell__ivypierlot_Ivys_MacBook_Pro_c
  wakatime["wakatime"]:::wakatime_c
  browsers__zen[/"browsers/zen"\]:::browsers__zen_c
  zotero["zotero"]:::zotero_c
  browsers__zen --> den__provides__unfree_libkey_nomad_onepassword_password_manager_
  dev --> dev_cli
  dev --> dev_nix
  fish --> user_shell__ivypierlot_Ivys_MacBook_Pro
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
  ivypierlot --> llama_cpp
  ivypierlot --> neovim
  ivypierlot --> nixvim
  ivypierlot --> nushell
  ivypierlot --> onepassword
  ivypierlot --> opencode
  ivypierlot --> paneru
  ivypierlot --> den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro_
  ivypierlot --> sketchybar
  ivypierlot --> browsers__zen
  main_ssh_key --> pam_rssh_user_ivypierlot
  neovim --> stylix
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  onepassword --> den__provides__unfree_onepassword_password_manager_
  opencode --> claude
  opencode --> mcp_servers
  end

  ivypierlot -.->|provides| ivypierlot__Ivys_MacBook_Pro__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef ccache__Ivys_MacBook_Pro_c fill:#bf8700,stroke:#bf8700,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef browsers_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ccache_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef claude_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef cotabby_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivypierlot_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef file_local_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef homebrew_host_Ivys_MacBook_Pro_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef homebrew_user_ivypierlot_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef idris_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef karabiner_driver_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef llama_cpp_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef mcp_servers_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef opencode_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_host_Ivys_MacBook_Pro_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef paneru_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_ivypierlot_Ivys_MacBook_Pro__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef shell_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sketchybar_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sudoagents_host_Ivys_MacBook_Pro_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sudoagents_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef ivypierlot__Ivys_MacBook_Pro__to_users_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_libkey_nomad_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_shell__ivypierlot_Ivys_MacBook_Pro_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef browsers__zen_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef zotero_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
style ctx_host_Ivys_MacBook_Pro fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_ivypierlot fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## auspc

**Architecture:** `x86_64-linux`
**Roles:** gui, gaming, dev

### Aspects
- agenix-rekey
- auscyber
- auscyber/auspc
- auspc/to-users
- batteries/define-user
- batteries/define-user/auscyber@auspc
- batteries/hostname
- batteries/hostname/os
- batteries/inputs&#39;
- batteries/inputs&#39;/os
- batteries/primary-user(auscyber@auspc)
- batteries/self&#39;
- batteries/self&#39;/os
- batteries/sources
- batteries/sources/os
- bootlogo
- builder-server
- cachyos-kernel
- ccache
- ccache/auspc
- celler-push
- claude
- den/provides/unfree(castlabs-electron)
- dev
- dev-cli
- dev-nix
- disko
- fish
- ghostty
- gpg
- gui
- home-base
- insecure-predicate
- insecure-predicate/os
- insecure-predicate/user
- ivy-fetch
- jujutsu
- lib
- main-ssh-key
- neovim
- nix
- nix-index
- nixos-general
- nixpkgs-config
- nixvim
- nushell
- onepassword
- openssh
- overlays
- packages
- packages/proton-ge-bin
- pam-rssh
- plasma
- provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)
- provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)
- provides/unfree(onepassword-password-manager)
- qemu
- qemu/to-users
- secure-boot
- shell
- shell/to-users
- starship
- stylix
- unfree-predicate
- unfree-predicate/os
- unfree-predicate/user
- user-shell/auscyber@auspc
- vpn
- vpn-secrets
- wakatime

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  auspc([auspc]):::root

  subgraph ctx_host_auspc["host: auspc"]
  agenix_rekey_host_auspc["agenix-rekey"]:::agenix_rekey_host_auspc_c
  ccache__auspc[/"ccache/auspc"\]:::ccache__auspc_c
  bootlogo["bootlogo"]:::bootlogo_c
  builder_server["builder-server"]:::builder_server_c
  cachyos_kernel["cachyos-kernel"]:::cachyos_kernel_c
  ccache["ccache"]:::ccache_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__auscyber_auspc{{"batteries/define-user/auscyber@auspc"}}:::den__batteries__define_user__auscyber_auspc_c
  disko["disko"]:::disko_c
  home_base["home-base"]:::home_base_c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_auspc["nixpkgs-config"]:::nixpkgs_config_host_auspc_c
  openssh["openssh"]:::openssh_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_auspc["pam-rssh"]:::pam_rssh_host_auspc_c
  qemu["qemu"]:::qemu_c
  secure_boot["secure-boot"]:::secure_boot_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  shell_host_auspc["shell"]:::shell_host_auspc_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  starship["starship"]:::starship_c
  auscyber__auspc__to_users[/"auspc/to-users"\]:::auscyber__auspc__to_users_c
  qemu__to_users[/"qemu/to-users"\]:::qemu__to_users_c
  den__provides__unfree_castlabs_electron__host_auspc{{"den/provides/unfree(castlabs-electron)"}}:::den__provides__unfree_castlabs_electron__host_auspc_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  auspc --> bootlogo
  auspc --> builder_server
  auspc --> cachyos_kernel
  auspc --> disko
  auspc --> qemu
  auspc --> secure_boot
  auspc --> den__provides__unfree_castlabs_electron__host_auspc
  auspc --> vpn
  cachyos_kernel --> ccache
  den__batteries__define_user --> den__batteries__define_user__auscyber_auspc
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__sources --> den__batteries__sources__os
  home_base --> shell_host_auspc
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  nixpkgs_config_host_auspc --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_auspc --> jujutsu
  shell_host_auspc --> nix_index
  shell_host_auspc --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> vpn_secrets
  vpn_secrets --> agenix_rekey_host_auspc
  ccache -.->|provides| ccache__auspc
  qemu -.->|provides| qemu__to_users
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber__auspc["auscyber/auspc"]:::auscyber__auspc_c
  celler_push["celler-push"]:::celler_push_c
  claude["claude"]:::claude_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_nix["dev-nix"]:::dev_nix_c
  fish["fish"]:::fish_c
  ghostty["ghostty"]:::ghostty_c
  gpg["gpg"]:::gpg_c
  gui["gui"]:::gui_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  neovim["neovim"]:::neovim_c
  nixvim["nixvim"]:::nixvim_c
  nushell["nushell"]:::nushell_c
  onepassword["onepassword"]:::onepassword_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  plasma["plasma"]:::plasma_c
  den__batteries__primary_user_auscyber_auspc_{{"batteries/primary-user(auscyber@auspc)"}}:::den__batteries__primary_user_auscyber_auspc__c
  packages__proton_ge_bin[/"packages/proton-ge-bin"\]:::packages__proton_ge_bin_c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_castlabs_electron__user_auscyber{{"den/provides/unfree(castlabs-electron)"}}:::den__provides__unfree_castlabs_electron__user_auscyber_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  user_shell__auscyber_auspc{{"user-shell/auscyber@auspc"}}:::user_shell__auscyber_auspc_c
  wakatime["wakatime"]:::wakatime_c
  auscyber --> celler_push
  auscyber --> claude
  auscyber --> fish
  auscyber --> den__provides__unfree_castlabs_electron__user_auscyber
  auscyber__auspc --> dev
  auscyber__auspc --> gpg
  auscyber__auspc --> gui
  auscyber__auspc --> neovim
  auscyber__auspc --> nushell
  auscyber__auspc --> plasma
  auscyber__auspc --> den__batteries__primary_user_auscyber_auspc_
  auscyber__auspc --> stylix
  celler_push --> agenix_rekey_user_auscyber
  dev --> dev_cli
  dev --> dev_nix
  fish --> user_shell__auscyber_auspc
  gui --> ghostty
  gui --> onepassword
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  onepassword --> den__provides__unfree_onepassword_password_manager_
  end

  auscyber -.->|provides| auscyber__auspc__to_users

  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef agenix_rekey_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber__auspc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef ccache__auspc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef bootlogo_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef builder_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef cachyos_kernel_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef ccache_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef claude_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__auscyber_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef disko_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef packages_c fill:#2da44e,stroke:#2da44e,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef pam_rssh_host_auspc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef plasma_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_auscyber_auspc__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef packages__proton_ge_bin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef qemu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef secure_boot_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef shell_host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef auscyber__auspc__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef qemu__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_castlabs_electron__host_auspc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_castlabs_electron__user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_shell__auscyber_auspc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_auspc fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_auscyber fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## lora-pi

**Architecture:** `aarch64-linux`

### Aspects
- agenix-rekey
- batteries/define-user
- batteries/define-user/ivy@lora-pi
- batteries/hostname
- batteries/hostname/os
- batteries/inputs&#39;
- batteries/inputs&#39;/os
- batteries/primary-user(ivy@lora-pi)
- batteries/self&#39;
- batteries/self&#39;/os
- batteries/sources
- batteries/sources/os
- fish
- gpg
- home-base
- insecure-predicate
- insecure-predicate/os
- insecure-predicate/user
- ivy
- ivy-fetch
- ivy/lora-pi
- jujutsu
- lib
- main-ssh-key
- neovim
- nix
- nix-index
- nixos-general
- nixos-raspberrypi
- nixpkgs-config
- nixvim
- openssh
- overlays
- pam-rssh
- provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)
- provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)
- shell
- shell/to-users
- starship
- stylix
- unfree-predicate
- unfree-predicate/os
- unfree-predicate/user
- user-shell/ivy@lora-pi
- wakatime

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  lora_pi([lora-pi]):::root

  subgraph ctx_host_lora_pi["host: lora-pi"]
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivy_lora_pi{{"batteries/define-user/ivy@lora-pi"}}:::den__batteries__define_user__ivy_lora_pi_c
  home_base["home-base"]:::home_base_c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixos_raspberrypi["nixos-raspberrypi"]:::nixos_raspberrypi_c
  nixpkgs_config_host_lora_pi["nixpkgs-config"]:::nixpkgs_config_host_lora_pi_c
  openssh["openssh"]:::openssh_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_lora_pi["pam-rssh"]:::pam_rssh_host_lora_pi_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  shell["shell"]:::shell_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  den__batteries__define_user --> den__batteries__define_user__ivy_lora_pi
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__sources --> den__batteries__sources__os
  home_base --> shell
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  lora_pi --> nixos_raspberrypi
  nixpkgs_config_host_lora_pi --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell --> jujutsu
  shell --> nix_index
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivy["user: ivy"]
  fish["fish"]:::fish_c
  gpg["gpg"]:::gpg_c
  ivy{{"ivy"}}:::ivy_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivy__lora_pi["ivy/lora-pi"]:::ivy__lora_pi_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  neovim["neovim"]:::neovim_c
  nixvim["nixvim"]:::nixvim_c
  pam_rssh_user_ivy["pam-rssh"]:::pam_rssh_user_ivy_c
  den__batteries__primary_user_ivy_lora_pi_{{"batteries/primary-user(ivy@lora-pi)"}}:::den__batteries__primary_user_ivy_lora_pi__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  user_shell__ivy_lora_pi{{"user-shell/ivy@lora-pi"}}:::user_shell__ivy_lora_pi_c
  wakatime["wakatime"]:::wakatime_c
  fish --> user_shell__ivy_lora_pi
  ivy__lora_pi --> fish
  ivy__lora_pi --> gpg
  ivy__lora_pi --> neovim
  ivy__lora_pi --> den__batteries__primary_user_ivy_lora_pi_
  main_ssh_key --> pam_rssh_user_ivy
  neovim --> nixvim
  neovim --> stylix
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivy_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivy__lora_pi_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos_raspberrypi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_host_lora_pi_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivy_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_ivy_lora_pi__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef shell_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_shell__ivy_lora_pi_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_lora_pi fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_ivy fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## macmini

**Architecture:** `aarch64-darwin`
**Roles:** gui

### Aspects
- agenix-rekey
- batteries/define-user
- batteries/define-user/ivypierlot@macmini
- batteries/hostname
- batteries/hostname/os
- batteries/inputs&#39;
- batteries/inputs&#39;/os
- batteries/primary-user(ivypierlot@macmini)
- batteries/self&#39;
- batteries/self&#39;/os
- batteries/sources
- batteries/sources/os
- browsers
- browsers/zen
- celler-push
- claude
- cotabby
- darwin-base
- darwin-finder
- darwin-general
- darwin-hmApps
- dev
- dev-cli
- dev-nix
- file-local
- fish
- ghostty
- gpg
- gui
- home-base
- homebrew
- idris
- insecure-predicate
- insecure-predicate/os
- insecure-predicate/user
- ivy-fetch
- ivypierlot
- jujutsu
- lib
- llama-cpp
- main-ssh-key
- mcp-servers
- neovim
- nix
- nix-index
- nixpkgs-config
- nixvim
- nushell
- onepassword
- opencode
- overlays
- pam-rssh
- pam-touchid
- paneru
- provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)
- provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)
- provides/unfree(libkey-nomad,onepassword-password-manager)
- provides/unfree(onepassword-password-manager)
- shell
- shell/to-users
- sketchybar
- starship
- stylix
- sudoagents
- unfree-predicate
- unfree-predicate/os
- unfree-predicate/user
- user-shell/ivypierlot@macmini
- wakatime

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  macmini([macmini]):::root

  subgraph ctx_host_macmini["host: macmini"]
  agenix_rekey_host_macmini["agenix-rekey"]:::agenix_rekey_host_macmini_c
  darwin_base["darwin-base"]:::darwin_base_c
  darwin_finder["darwin-finder"]:::darwin_finder_c
  darwin_general["darwin-general"]:::darwin_general_c
  darwin_hmApps["darwin-hmApps"]:::darwin_hmApps_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__ivypierlot_macmini{{"batteries/define-user/ivypierlot@macmini"}}:::den__batteries__define_user__ivypierlot_macmini_c
  home_base["home-base"]:::home_base_c
  homebrew_host_macmini["homebrew"]:::homebrew_host_macmini_c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nixpkgs_config_host_macmini["nixpkgs-config"]:::nixpkgs_config_host_macmini_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_macmini["pam-rssh"]:::pam_rssh_host_macmini_c
  pam_touchid["pam-touchid"]:::pam_touchid_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  shell_host_macmini["shell"]:::shell_host_macmini_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  darwin_base --> darwin_finder
  darwin_base --> darwin_general
  darwin_base --> darwin_hmApps
  darwin_base --> pam_touchid
  den__batteries__define_user --> den__batteries__define_user__ivypierlot_macmini
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__sources --> den__batteries__sources__os
  home_base --> shell_host_macmini
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  macmini --> homebrew_host_macmini
  nixpkgs_config_host_macmini --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_macmini --> jujutsu
  shell_host_macmini --> nix_index
  shell_host_macmini --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_ivypierlot["user: ivypierlot"]
  agenix_rekey_user_ivypierlot["agenix-rekey"]:::agenix_rekey_user_ivypierlot_c
  celler_push["celler-push"]:::celler_push_c
  claude["claude"]:::claude_c
  cotabby["cotabby"]:::cotabby_c
  dev["dev"]:::dev_c
  dev_cli["dev-cli"]:::dev_cli_c
  dev_nix["dev-nix"]:::dev_nix_c
  file_local["file-local"]:::file_local_c
  fish["fish"]:::fish_c
  ghostty["ghostty"]:::ghostty_c
  gpg["gpg"]:::gpg_c
  gui["gui"]:::gui_c
  homebrew_user_ivypierlot["homebrew"]:::homebrew_user_ivypierlot_c
  idris["idris"]:::idris_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  ivypierlot{{"ivypierlot"}}:::ivypierlot_c
  llama_cpp["llama-cpp"]:::llama_cpp_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  mcp_servers["mcp-servers"]:::mcp_servers_c
  neovim["neovim"]:::neovim_c
  nixvim["nixvim"]:::nixvim_c
  nushell["nushell"]:::nushell_c
  onepassword["onepassword"]:::onepassword_c
  opencode["opencode"]:::opencode_c
  pam_rssh_user_ivypierlot["pam-rssh"]:::pam_rssh_user_ivypierlot_c
  paneru["paneru"]:::paneru_c
  den__batteries__primary_user_ivypierlot_macmini_{{"batteries/primary-user(ivypierlot@macmini)"}}:::den__batteries__primary_user_ivypierlot_macmini__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  sketchybar["sketchybar"]:::sketchybar_c
  stylix["stylix"]:::stylix_c
  sudoagents["sudoagents"]:::sudoagents_c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  den__provides__unfree_libkey_nomad_onepassword_password_manager_{{"provides/unfree(libkey-nomad,onepassword-password-manager)"}}:::den__provides__unfree_libkey_nomad_onepassword_password_manager__c
  den__provides__unfree_onepassword_password_manager_{{"provides/unfree(onepassword-password-manager)"}}:::den__provides__unfree_onepassword_password_manager__c
  user_shell__ivypierlot_macmini{{"user-shell/ivypierlot@macmini"}}:::user_shell__ivypierlot_macmini_c
  wakatime["wakatime"]:::wakatime_c
  browsers__zen[/"browsers/zen"\]:::browsers__zen_c
  browsers__zen --> den__provides__unfree_libkey_nomad_onepassword_password_manager_
  dev --> dev_cli
  dev --> dev_nix
  fish --> user_shell__ivypierlot_macmini
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
  ivypierlot --> llama_cpp
  ivypierlot --> neovim
  ivypierlot --> nixvim
  ivypierlot --> nushell
  ivypierlot --> onepassword
  ivypierlot --> opencode
  ivypierlot --> paneru
  ivypierlot --> den__batteries__primary_user_ivypierlot_macmini_
  ivypierlot --> sketchybar
  ivypierlot --> browsers__zen
  main_ssh_key --> pam_rssh_user_ivypierlot
  neovim --> stylix
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  onepassword --> den__provides__unfree_onepassword_password_manager_
  opencode --> claude
  opencode --> mcp_servers
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef agenix_rekey_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_ivypierlot_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef browsers_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef claude_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef cotabby_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_base_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef darwin_finder_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef darwin_general_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef darwin_hmApps_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__ivypierlot_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef dev_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_cli_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef dev_nix_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef file_local_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef ghostty_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef gui_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef homebrew_host_macmini_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef homebrew_user_ivypierlot_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef idris_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef llama_cpp_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef mcp_servers_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef nushell_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef onepassword_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef opencode_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_host_macmini_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_ivypierlot_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pam_touchid_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef paneru_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_ivypierlot_macmini__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef shell_host_macmini_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef sketchybar_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef sudoagents_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_libkey_nomad_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_onepassword_password_manager__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_shell__ivypierlot_macmini_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef browsers__zen_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
style ctx_host_macmini fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_ivypierlot fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## pentestvm

**Architecture:** `x86_64-linux`

### Aspects
- admin
- agenix-rekey
- batteries/define-user
- batteries/define-user/admin@pentestvm
- batteries/hostname
- batteries/hostname/os
- batteries/inputs&#39;
- batteries/inputs&#39;/os
- batteries/primary-user(admin@pentestvm)
- batteries/self&#39;
- batteries/self&#39;/os
- batteries/sources
- batteries/sources/os
- home-base
- insecure-predicate
- insecure-predicate/os
- insecure-predicate/user
- ivy-fetch
- jujutsu
- lib
- main-ssh-key
- nix
- nix-index
- nixos-general
- nixpkgs-config
- openssh
- overlays
- pam-rssh
- provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)
- shell
- shell/to-users
- starship
- unfree-predicate
- unfree-predicate/os
- unfree-predicate/user

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  pentestvm([pentestvm]):::root

  subgraph ctx_host_pentestvm["host: pentestvm"]
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__admin_pentestvm{{"batteries/define-user/admin@pentestvm"}}:::den__batteries__define_user__admin_pentestvm_c
  home_base["home-base"]:::home_base_c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_pentestvm["nixpkgs-config"]:::nixpkgs_config_host_pentestvm_c
  openssh["openssh"]:::openssh_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_pentestvm["pam-rssh"]:::pam_rssh_host_pentestvm_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  shell["shell"]:::shell_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  den__batteries__define_user --> den__batteries__define_user__admin_pentestvm
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__sources --> den__batteries__sources__os
  home_base --> shell
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  nixpkgs_config_host_pentestvm --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell --> jujutsu
  shell --> nix_index
  shell --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_admin["user: admin"]
  admin{{"admin"}}:::admin_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  pam_rssh_user_admin["pam-rssh"]:::pam_rssh_user_admin_c
  den__batteries__primary_user_admin_pentestvm_{{"batteries/primary-user(admin@pentestvm)"}}:::den__batteries__primary_user_admin_pentestvm__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  admin --> den__batteries__primary_user_admin_pentestvm_
  main_ssh_key --> pam_rssh_user_admin
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef admin_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__admin_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_host_pentestvm_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_admin_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef pentestvm_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_admin_pentestvm__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef shell_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
style ctx_host_pentestvm fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_admin fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## secondpc

**Architecture:** `x86_64-linux`

### Aspects
- agenix-rekey
- auscyber
- auscyber/secondpc
- batteries/define-user
- batteries/define-user/auscyber@secondpc
- batteries/hostname
- batteries/hostname/os
- batteries/inputs&#39;
- batteries/inputs&#39;/os
- batteries/primary-user(auscyber@secondpc)
- batteries/self&#39;
- batteries/self&#39;/os
- batteries/sources
- batteries/sources/os
- builder-server
- builders
- celler
- celler-push
- claude
- disko
- fish
- gpg
- home-base
- insecure-predicate
- insecure-predicate/os
- insecure-predicate/user
- ivy-fetch
- jujutsu
- lib
- local
- main-ssh-key
- neovim
- nginx
- nix
- nix-index
- nix/secondpc
- nixos-general
- nixpkgs-config
- nixvim
- openssh
- overlays
- pam-rssh
- provides/unfree(castlabs-electron)
- provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)
- provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)
- provides/unfree(intel-ocl)
- searchix
- secondpc-web
- shell
- shell/to-users
- starship
- stylix
- unfree-predicate
- unfree-predicate/os
- unfree-predicate/user
- user-shell/auscyber@secondpc
- vpn
- vpn-secrets
- vpn-server
- wakatime

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  secondpc([secondpc]):::root

  subgraph ctx_host_secondpc["host: secondpc"]
  agenix_rekey_host_secondpc["agenix-rekey"]:::agenix_rekey_host_secondpc_c
  builder_server["builder-server"]:::builder_server_c
  builders["builders"]:::builders_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__auscyber_secondpc{{"batteries/define-user/auscyber@secondpc"}}:::den__batteries__define_user__auscyber_secondpc_c
  disko["disko"]:::disko_c
  home_base["home-base"]:::home_base_c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  lib["lib"]:::lib_c
  local["local"]:::local_c
  nginx_host_secondpc["nginx"]:::nginx_host_secondpc_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_secondpc["nixpkgs-config"]:::nixpkgs_config_host_secondpc_c
  openssh["openssh"]:::openssh_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_secondpc["pam-rssh"]:::pam_rssh_host_secondpc_c
  searchix["searchix"]:::searchix_c
  secondpc_web["secondpc-web"]:::secondpc_web_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  shell_host_secondpc["shell"]:::shell_host_secondpc_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  den__provides__unfree_intel_ocl_{{"provides/unfree(intel-ocl)"}}:::den__provides__unfree_intel_ocl__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  vpn["vpn"]:::vpn_c
  vpn_secrets["vpn-secrets"]:::vpn_secrets_c
  vpn_server["vpn-server"]:::vpn_server_c
  den__batteries__define_user --> den__batteries__define_user__auscyber_secondpc
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__sources --> den__batteries__sources__os
  home_base --> shell_host_secondpc
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  nixpkgs_config_host_secondpc --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  secondpc --> builder_server
  secondpc --> builders
  secondpc --> disko
  secondpc --> local
  secondpc --> nginx_host_secondpc
  secondpc --> nix
  secondpc --> searchix
  secondpc --> secondpc_web
  secondpc --> den__provides__unfree_intel_ocl_
  secondpc --> vpn_server
  shell_host_secondpc --> jujutsu
  shell_host_secondpc --> nix_index
  shell_host_secondpc --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  vpn --> vpn_secrets
  vpn_secrets --> agenix_rekey_host_secondpc
  vpn_server --> vpn
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber__secondpc["auscyber/secondpc"]:::auscyber__secondpc_c
  celler["celler"]:::celler_c
  celler_push["celler-push"]:::celler_push_c
  claude["claude"]:::claude_c
  fish["fish"]:::fish_c
  gpg["gpg"]:::gpg_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  neovim["neovim"]:::neovim_c
  nginx_user_auscyber["nginx"]:::nginx_user_auscyber_c
  nix__secondpc["nix/secondpc"]:::nix__secondpc_c
  nixvim["nixvim"]:::nixvim_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  den__batteries__primary_user_auscyber_secondpc_{{"batteries/primary-user(auscyber@secondpc)"}}:::den__batteries__primary_user_auscyber_secondpc__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_castlabs_electron_{{"provides/unfree(castlabs-electron)"}}:::den__provides__unfree_castlabs_electron__c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  user_shell__auscyber_secondpc{{"user-shell/auscyber@secondpc"}}:::user_shell__auscyber_secondpc_c
  wakatime["wakatime"]:::wakatime_c
  auscyber --> celler_push
  auscyber --> claude
  auscyber --> fish
  auscyber --> den__provides__unfree_castlabs_electron_
  auscyber__secondpc --> gpg
  auscyber__secondpc --> neovim
  auscyber__secondpc --> den__batteries__primary_user_auscyber_secondpc_
  celler_push --> agenix_rekey_user_auscyber
  fish --> user_shell__auscyber_secondpc
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  neovim --> stylix
  nix__secondpc --> celler
  nix__secondpc --> nginx_user_auscyber
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef agenix_rekey_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber__secondpc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef builder_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef builders_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef celler_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef claude_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__auscyber_secondpc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef disko_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef local_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nginx_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nginx_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nix__secondpc_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_host_secondpc_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_auscyber_secondpc__c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef searchix_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef secondpc_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef secondpc_web_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef shell_host_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_castlabs_electron__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_intel_ocl__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_shell__auscyber_secondpc_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef vpn_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef vpn_secrets_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef vpn_server_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_secondpc fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_auscyber fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## surfacelaptop

**Architecture:** `x86_64-linux`
**Roles:** gui, dev

### Aspects
- agenix-rekey
- auscyber
- auscyber/surfacelaptop
- batteries/define-user
- batteries/define-user/auscyber@surfacelaptop
- batteries/hostname
- batteries/hostname/os
- batteries/inputs&#39;
- batteries/inputs&#39;/os
- batteries/primary-user(auscyber@surfacelaptop)
- batteries/self&#39;
- batteries/self&#39;/os
- batteries/sources
- batteries/sources/os
- celler-push
- claude
- fish
- gpg
- home-base
- insecure-predicate
- insecure-predicate/os
- insecure-predicate/user
- ivy-fetch
- jujutsu
- lib
- main-ssh-key
- neovim
- nix
- nix-index
- nixos-general
- nixpkgs-config
- nixvim
- openssh
- overlays
- pam-rssh
- provides/unfree(castlabs-electron)
- provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)
- provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)
- shell
- shell/to-users
- starship
- stylix
- unfree-predicate
- unfree-predicate/os
- unfree-predicate/user
- user-shell/auscyber@surfacelaptop
- wakatime

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  surfacelaptop([surfacelaptop]):::root

  subgraph ctx_host_surfacelaptop["host: surfacelaptop"]
  agenix_rekey_host_surfacelaptop["agenix-rekey"]:::agenix_rekey_host_surfacelaptop_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__auscyber_surfacelaptop{{"batteries/define-user/auscyber@surfacelaptop"}}:::den__batteries__define_user__auscyber_surfacelaptop_c
  home_base["home-base"]:::home_base_c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_surfacelaptop["nixpkgs-config"]:::nixpkgs_config_host_surfacelaptop_c
  openssh["openssh"]:::openssh_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_surfacelaptop["pam-rssh"]:::pam_rssh_host_surfacelaptop_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  shell_host_surfacelaptop["shell"]:::shell_host_surfacelaptop_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  den__batteries__define_user --> den__batteries__define_user__auscyber_surfacelaptop
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__sources --> den__batteries__sources__os
  home_base --> shell_host_surfacelaptop
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  nixpkgs_config_host_surfacelaptop --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_surfacelaptop --> jujutsu
  shell_host_surfacelaptop --> nix_index
  shell_host_surfacelaptop --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_auscyber["user: auscyber"]
  agenix_rekey_user_auscyber["agenix-rekey"]:::agenix_rekey_user_auscyber_c
  auscyber{{"auscyber"}}:::auscyber_c
  auscyber__surfacelaptop["auscyber/surfacelaptop"]:::auscyber__surfacelaptop_c
  celler_push["celler-push"]:::celler_push_c
  claude["claude"]:::claude_c
  fish["fish"]:::fish_c
  gpg["gpg"]:::gpg_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  neovim["neovim"]:::neovim_c
  nixvim["nixvim"]:::nixvim_c
  pam_rssh_user_auscyber["pam-rssh"]:::pam_rssh_user_auscyber_c
  den__batteries__primary_user_auscyber_surfacelaptop_{{"batteries/primary-user(auscyber@surfacelaptop)"}}:::den__batteries__primary_user_auscyber_surfacelaptop__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  stylix["stylix"]:::stylix_c
  den__provides__unfree_castlabs_electron_{{"provides/unfree(castlabs-electron)"}}:::den__provides__unfree_castlabs_electron__c
  den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_{{"provides/unfree(cmp-nvim-lsp-document-symbol,cmp-copilot)"}}:::den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c
  user_shell__auscyber_surfacelaptop{{"user-shell/auscyber@surfacelaptop"}}:::user_shell__auscyber_surfacelaptop_c
  wakatime["wakatime"]:::wakatime_c
  auscyber --> celler_push
  auscyber --> claude
  auscyber --> fish
  auscyber --> den__provides__unfree_castlabs_electron_
  auscyber__surfacelaptop --> gpg
  auscyber__surfacelaptop --> neovim
  auscyber__surfacelaptop --> den__batteries__primary_user_auscyber_surfacelaptop_
  celler_push --> agenix_rekey_user_auscyber
  fish --> user_shell__auscyber_surfacelaptop
  main_ssh_key --> pam_rssh_user_auscyber
  neovim --> nixvim
  neovim --> stylix
  nixvim --> den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot_
  nixvim --> wakatime
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef agenix_rekey_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef agenix_rekey_user_auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef auscyber__surfacelaptop_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef celler_push_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef claude_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__auscyber_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef gpg_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef neovim_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixpkgs_config_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef nixvim_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_host_surfacelaptop_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_auscyber_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_auscyber_surfacelaptop__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef shell_host_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef stylix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_castlabs_electron__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_cmp_nvim_lsp_document_symbol_cmp_copilot__c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:2px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_shell__auscyber_surfacelaptop_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef wakatime_c fill:#fa4549,stroke:#fa4549,color:#1f2328,stroke-width:3px
style ctx_host_surfacelaptop fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_auscyber fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```

## wsl-nixos

**Architecture:** `x86_64-linux`

### Aspects
- agenix-rekey
- batteries/define-user
- batteries/define-user/nixos@wsl-nixos
- batteries/hostname
- batteries/hostname/os
- batteries/inputs&#39;
- batteries/inputs&#39;/os
- batteries/primary-user(nixos@wsl-nixos)
- batteries/self&#39;
- batteries/self&#39;/os
- batteries/sources
- batteries/sources/os
- fish
- home-base
- insecure-predicate
- insecure-predicate/os
- insecure-predicate/user
- ivy-fetch
- jujutsu
- lib
- main-ssh-key
- nix
- nix-index
- nixos
- nixos-general
- nixos/to-users
- nixpkgs-config
- openssh
- overlays
- pam-rssh
- provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)
- shell
- shell/to-users
- starship
- unfree-predicate
- unfree-predicate/os
- unfree-predicate/user
- user-shell/nixos@wsl-nixos

### Aspect Graph

```mermaid
%%{init: {"theme":"base","themeVariables":{"activationBkgColor":"#d0d7de","activationBorderColor":"#8c959f","actorBkg":"#d0d7de","actorBorder":"#6e7781","actorLineColor":"#6e7781","actorTextColor":"#424a53","background":"#eaeef2","classText":"#424a53","clusterBkg":"#d0d7de","clusterBorder":"#8c959f","edgeLabelBackground":"#eaeef2","labelBoxBkgColor":"#d0d7de","labelBoxBorderColor":"#6e7781","labelTextColor":"#424a53","lineColor":"#6e7781","loopTextColor":"#424a53","mainBkg":"#d0d7de","nodeBkg":"#d0d7de","nodeBorder":"#6e7781","nodeTextColor":"#424a53","noteBkgColor":"#d0d7de","noteBorderColor":"#8c959f","noteTextColor":"#424a53","pie1":"#fa4549","pie2":"#e16f24","pie3":"#bf8700","pie4":"#2da44e","pie5":"#339D9B","pie6":"#218bff","pie7":"#a475f9","pie8":"#4d2d00","pieLegendTextColor":"#424a53","pieOuterStrokeColor":"#8c959f","pieSectionTextColor":"#424a53","pieStrokeColor":"#8c959f","pieTitleTextColor":"#424a53","primaryBorderColor":"#6e7781","primaryColor":"#d0d7de","primaryTextColor":"#424a53","secondBkg":"#d0d7de","secondaryBorderColor":"#8c959f","secondaryColor":"#d0d7de","secondaryTextColor":"#424a53","sequenceNumberColor":"#eaeef2","signalColor":"#6e7781","signalTextColor":"#424a53","tertiaryBorderColor":"#8c959f","tertiaryColor":"#d0d7de","tertiaryTextColor":"#424a53","textColor":"#424a53","titleColor":"#424a53"}}}%%
graph LR
  wsl_nixos([wsl-nixos]):::root

  subgraph ctx_host_wsl_nixos["host: wsl-nixos"]
  agenix_rekey["agenix-rekey"]:::agenix_rekey_c
  den__batteries__define_user[/"batteries/define-user"\]:::den__batteries__define_user_c
  den__batteries__define_user__nixos_wsl_nixos{{"batteries/define-user/nixos@wsl-nixos"}}:::den__batteries__define_user__nixos_wsl_nixos_c
  home_base["home-base"]:::home_base_c
  den__batteries__hostname[/"batteries/hostname"\]:::den__batteries__hostname_c
  den__batteries__hostname__os{{"batteries/hostname/os"}}:::den__batteries__hostname__os_c
  den__batteries__inputs_[/"batteries/inputs'"\]:::den__batteries__inputs__c
  den__batteries__inputs___os{{"batteries/inputs'/os"}}:::den__batteries__inputs___os_c
  insecure_predicate["insecure-predicate"]:::insecure_predicate_c
  insecure_predicate__os{{"insecure-predicate/os"}}:::insecure_predicate__os_c
  insecure_predicate__user{{"insecure-predicate/user"}}:::insecure_predicate__user_c
  jujutsu["jujutsu"]:::jujutsu_c
  lib["lib"]:::lib_c
  nix["nix"]:::nix_c
  nix_index["nix-index"]:::nix_index_c
  nixos_general["nixos-general"]:::nixos_general_c
  nixpkgs_config_host_wsl_nixos["nixpkgs-config"]:::nixpkgs_config_host_wsl_nixos_c
  openssh["openssh"]:::openssh_c
  overlays["overlays"]:::overlays_c
  pam_rssh_host_wsl_nixos["pam-rssh"]:::pam_rssh_host_wsl_nixos_c
  den__batteries__self_[/"batteries/self'"\]:::den__batteries__self__c
  den__batteries__self___os{{"batteries/self'/os"}}:::den__batteries__self___os_c
  shell_host_wsl_nixos["shell"]:::shell_host_wsl_nixos_c
  den__batteries__sources[/"batteries/sources"\]:::den__batteries__sources_c
  den__batteries__sources__os{{"batteries/sources/os"}}:::den__batteries__sources__os_c
  starship["starship"]:::starship_c
  den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_{{"provides/unfree(copilot.vim,idris2-vim,presence.nvim,cmp-copilot,intel-ocl,code,1password,1password-cli,1password-gui,1password-gui-beta,claude-code,discord,google-chrome,helium,helium-bin,libkey-nomad,memorymate,minecraft-launcher,minecraft-server,nvidia-settings,nvidia-x11,obsidian,opencode,slack,spotify,steam,steam-original,steam-run,steam-runtime,steam-unwrapped,tidal-hifi,vscode,zoom,cmp-nvim-lsp-document-symbol)"}}:::den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c
  unfree_predicate["unfree-predicate"]:::unfree_predicate_c
  unfree_predicate__os{{"unfree-predicate/os"}}:::unfree_predicate__os_c
  unfree_predicate__user{{"unfree-predicate/user"}}:::unfree_predicate__user_c
  den__batteries__define_user --> den__batteries__define_user__nixos_wsl_nixos
  den__batteries__hostname --> den__batteries__hostname__os
  den__batteries__inputs_ --> den__batteries__inputs___os
  den__batteries__self_ --> den__batteries__self___os
  den__batteries__sources --> den__batteries__sources__os
  home_base --> shell_host_wsl_nixos
  insecure_predicate --> insecure_predicate__os
  insecure_predicate --> insecure_predicate__user
  nixpkgs_config_host_wsl_nixos --> den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol_
  shell_host_wsl_nixos --> jujutsu
  shell_host_wsl_nixos --> nix_index
  shell_host_wsl_nixos --> starship
  unfree_predicate --> unfree_predicate__os
  unfree_predicate --> unfree_predicate__user
  end
  subgraph ctx_user_nixos["user: nixos"]
  fish["fish"]:::fish_c
  ivy_fetch["ivy-fetch"]:::ivy_fetch_c
  main_ssh_key["main-ssh-key"]:::main_ssh_key_c
  nixos{{"nixos"}}:::nixos_c
  nixos__to_users["nixos/to-users"]:::nixos__to_users_c
  pam_rssh_user_nixos["pam-rssh"]:::pam_rssh_user_nixos_c
  den__batteries__primary_user_nixos_wsl_nixos_{{"batteries/primary-user(nixos@wsl-nixos)"}}:::den__batteries__primary_user_nixos_wsl_nixos__c
  shell__to_users["shell/to-users"]:::shell__to_users_c
  user_shell__nixos_wsl_nixos{{"user-shell/nixos@wsl-nixos"}}:::user_shell__nixos_wsl_nixos_c
  fish --> user_shell__nixos_wsl_nixos
  main_ssh_key --> pam_rssh_user_nixos
  nixos --> fish
  nixos --> den__batteries__primary_user_nixos_wsl_nixos_
  end


  classDef root fill:#218bff,stroke:#218bff,color:#1f2328,font-weight:bold
  classDef agenix_rekey_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__define_user__nixos_wsl_nixos_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef fish_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef home_base_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__hostname__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef den__batteries__inputs__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__inputs___os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef insecure_predicate__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef insecure_predicate__user_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef ivy_fetch_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef jujutsu_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef lib_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef main_ssh_key_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:3px
  classDef nix_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nix_index_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef nixos_general_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef nixos__to_users_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef nixpkgs_config_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef openssh_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef overlays_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_host_wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef pam_rssh_user_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef den__batteries__primary_user_nixos_wsl_nixos__c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef den__batteries__self__c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__batteries__self___os_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:2px
  classDef shell_host_wsl_nixos_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef shell__to_users_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px,stroke-dasharray: 8 4
  classDef den__batteries__sources_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
  classDef den__batteries__sources__os_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef starship_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:3px
  classDef den__provides__unfree_copilot_vim_idris2_vim_presence_nvim_cmp_copilot_intel_ocl_code_1password_1password_cli_1password_gui_1password_gui_beta_claude_code_discord_google_chrome_helium_helium_bin_libkey_nomad_memorymate_minecraft_launcher_minecraft_server_nvidia_settings_nvidia_x11_obsidian_opencode_slack_spotify_steam_steam_original_steam_run_steam_runtime_steam_unwrapped_tidal_hifi_vscode_zoom_cmp_nvim_lsp_document_symbol__c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:2px
  classDef unfree_predicate_c fill:#4d2d00,stroke:#4d2d00,color:#1f2328,stroke-width:3px
  classDef unfree_predicate__os_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-width:2px
  classDef unfree_predicate__user_c fill:#a475f9,stroke:#a475f9,color:#1f2328,stroke-dasharray: 3 3,stroke-width:1px
  classDef user_shell__nixos_wsl_nixos_c fill:#e16f24,stroke:#e16f24,color:#1f2328,stroke-width:2px
  classDef wsl_nixos_c fill:#218bff,stroke:#218bff,color:#1f2328,stroke-width:3px
style ctx_host_wsl_nixos fill:#d0d7de,stroke:#8c959f,stroke-width:2px
style ctx_user_nixos fill:#d0d7de,stroke:#8c959f,stroke-width:2px
```


## My Packages

| Package | Version | Description |
|---------|---------|-------------|
| celler | `0.1.0` | Multi-tenant Nix binary cache system |
| cotabby | `v0.6.2-beta` |  |
| ghostty | `1.3.1` | Fast, native, feature-rich terminal emulator pushing modern features |
| helium | `0.15.2.1` | Private, fast, and honest web browser based on Chromium |
| ivy-fetch | `unknown` |  |
| jankyborders | `a7297ca7d1933f3a30b12e8f10750e8d84eeee1e` | Lightweight tool designed to add colored borders to user windows on macOS 14.0+ |
| jj-mcp-server | `1.0.1` | Model Context Protocol server for the Jujutsu (jj) version control system |
| kanata-ls | `160f6af8b415ad77dd1ecde8a95d7e93b75d9095` | Kanata Language Server |
| kanata-tray | `v0.8.0` | Tray Icon for Kanata  |
| lspmux | `18861f9d59e74ece8d867772cf07fa302c2dae98` |  |
| proton-ge-bin | `GE-Proton11-3` | Compatibility tool for Steam Play based on Wine and additional components.

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
