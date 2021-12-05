(module plugins.distant
  {require {distant distant}
   autoload {distant-settings distant.settings}})

(distant.setup {
                :* (distant-settings.chip_default)})

