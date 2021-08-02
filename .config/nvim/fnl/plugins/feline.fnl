(module plugins.feline
  {require {feline feline
            colors colors
            lsp feline.providers.lsp
            vi_mode_utils feline.providers.vi_mode}})


(local b vim.b)
(local fnn vim.fn)

(local properties {
                   :force_inactive {
                                    :filetypes  {}
                                    :buftypes  {}
                                    :bufnames  {}}})


(local components {
                   :left {
                          :active {}
                          :inactive {}}
                   :mid {
                         :active {}
                         :inactive {}}
                   :right {
                           :active {}
                           :inactive {}}})


(set properties.force_inactive.filetypes [
                                          "NvimTree"
                                          "dbui"
                                          "packer"
                                          "startify"
                                          "fugitive"
                                          "fugitiveblame"])


(set properties.force_inactive.buftypes [ "terminal"])

(tset components.left :active
     [
      {
       :provider " "
       :hl {
            :fg colors.cyan
            :bg "NONE"}}


      {
       :provider  (fn [] "  ")
       :hl (fn []
            (local val {:bg colors.cyan})
            (set val.name (vi_mode_utils.get_mode_highlight_name))
            (set val.fg (vi_mode_utils.get_mode_color))
            (set val.style "bold")
            val)
       :right_sep {:str " " :hl {:bg colors.cyan}}}


      {
       :provider  "file_info"
       :type :relative-short
       :hl {
            :fg "white"
            :bg colors.grey
            :style "bold"}
       :left_sep  [{:str  " " :hl {:bg colors.grey}}] 
;                                             "slant_left_2" {:str  " " :hl  {:bg colors.grey :fg  "NONE"}}]
        :right_sep  [" "]}



      {:provider  "file_size"
        :enabled (fn [] (> (fnn.getfsize (fnn.expand "%:p")) 0))
        :right_sep  [" " {:str :right_rounded :hl {:fg  :bg :bg  :NONE}}]}])
(tset components.mid :active
      [
        {
         :provider :lsp_client_names
         :hl {:fg colors.white :bg colors.grey}
         :enabled (fn [] (> (length (vim.lsp.get_active_clients)) 0))
         :right_sep  [{:str :right_rounded  :hl {:fg colors.grey :bg :NONE}}]
         :left_sep  [{:str :left_rounded :hl {:fg colors.grey :bg :NONE}}]}])


(tset components.right :active
    [
      

      {

        :provider  "position"
        :hl {:bg colors.light_red :fg colors.white :style :bold} 
        :right_sep  [{:str " " :hl {:bg colors.light_red}}]
        :left_sep  [{:str :left_rounded :hl {:fg colors.light_red :bg :NONE}}]}


      {
                        :provider  "git_branch"
                        :hl  {
                               :fg  "white"
                               :bg  "black"
                               :style  "bold"}
                        :right_sep  (fn []
                                     (local val  {:hl  {:fg  "NONE" :bg  "black"}})
                                     (if b.gitsigns_status_dict (set val.str " ") (set val.str ""))
                                     val)}


      {
          :provider  "git_diff_added"
                                   :hl  {
                                         :fg  "green"
                                         :bg  "black"}}


      {
       :provider  "git_diff_changed"
       :hl  {
             :fg  "orange"
             :bg  "black"}}


      {
       :provider  "git_diff_removed"
       :hl  {
             :fg  "red"
             :bg  "black"}
       :right_sep  (fn []
                    (local val  {:hl  {:fg  "NONE" :bg  "black"}})
                    (if b.gitsigns_status_dict (set val.str  " ") (set val.str  ""))
                    val)}




      {
       :provider  "line_percentage"
       :hl  {
             :style  "bold"}
       :left_sep  "  "
       :right_sep  " "}


      {
       :provider  "scroll_bar"
       :hl  {
             :fg  colors.light_red
             :style  "bold"}}


      {
        :provider  "diagnostic_errors"
        :enabled  (fn [] (lsp.diagnostics_exist "Error"))
        :hl  { :fg  "red"}}


      {
        :provider  "diagnostic_warnings"
        :enabled  (fn [] (lsp.diagnostics_exist "Warning"))
        :hl  { :fg  "yellow"}}


      {
        :provider  "diagnostic_hints"
        :enabled  (fn [] (lsp.diagnostics_exist "Hint"))
        :hl  { :fg  "cyan"}}


      {
                :provider  "diagnostic_info"
                :enabled  (fn [] (lsp.diagnostics_exist "Information"))
                :hl  { :fg  "skyblue"}}
      {
       :provider " "
       :right_sep {:str :right_rounded :hl {:fg :bg :bg :NONE}}}])



(tset components.left :inactive
    [
      {
       :provider  "file_type"
       :hl  {
              :fg  "white"
              :bg  colors.cyan}
       :left_sep  [{:str " " :hl {:fg :NONE :bg :NONE}} :left_rounded {
                                                                       :str  " "
                                                                       :hl  {
                                                                              :fg  :NONE
                                                                              :bg  colors.cyan}}]
       :right_sep {:str " " :hl {:bg colors.cyan}}}
      {
        :provider  "file_info"
        :hl  {
               :fg  "white"
               :bg  colors.dark_cyan
               :style  "bold"}
        :left_sep {:str " " :hl {:bg colors.dark_cyan}}
        :right_sep  [{:str  " "
                         :hl  {:fg  "NONE"
                                :bg  colors.dark_cyan}} {:str "right_rounded" :hl {:bg "NONE" :fg colors.dark_cyan}}]}])




-- This table is equal to the default colors table
(local colors {
               :black  "#1B1B1B"
               :skyblue  "#50B0F0"
               :cyan  "#009090"
               :green  "#60A040"
               :oceanblue  "#0066cc"
               :magenta  "#C26BDB"
               :orange  "#FF9000"
               :red  "#D10000"
               :violet  "#9E93E8"
               :white  "#FFFFFF"
               :yellow  "#E1E120"})


-- This table is equal to the default separators table
(local separators {
                   :vertical_bar  "┃"
                   :vertical_bar_thin  "│"
                   :left  ""
                   :right  ""
                   :block  "█"
                   :left_filled  ""
                   :right_filled  ""
                   :slant_left  ""
                   :slant_left_thin  ""
                   :slant_right  ""
                   :slant_right_thin  ""
                   :slant_left_2  ""
                   :slant_left_2_thin  ""
                   :slant_right_2  ""
                   :slant_right_2_thin  ""
                   :left_rounded  ""
                   :left_rounded_thin  ""
                   :right_rounded  ""
                   :right_rounded_thin  ""
                   :circle  "●"})


-- This table is equal to the default vi_mode_colors table
(local vi_mode_colors {
                       :NORMAL colors.white
                       :OP  "green"
                       :INSERT  colors.dark_cyan
                       :VISUAL  colors.blue
                       :BLOCK  colors.blue
                       :REPLACE  colors.red
                       ["V-REPLACE"]  "violet"
                       :ENTER  "cyan"
                       :MORE  "cyan"
                       :SELECT  "orange"
                       :COMMAND  colors.cyan
                       :SHELL  "green"
                       :TERM  "green"
                       :NONE  "yellow"})


(feline.setup {
               :default_fg "#D0D0D0"
               :default_bg colors.purple
               :colors colors
               :separators separators
               :components components
               :properties properties
               :vi_mode_colors vi_mode_colors})

