local M = {}

M.map = {
  ['n']    = 'No',
  ['no']   = 'O-PENDING',
  ['nov']  = 'O-PENDING',
  ['noV']  = 'O-PENDING',
  ['no'] = 'O-PENDING',
  ['niI']  = 'No',
  ['niR']  = 'No',
  ['niV']  = 'No',
  ['v']    = 'Vv',
  ['V']    = 'VL',
  ['']   = 'Vb',
  ['s']    = 'Ss',
  ['S']    = 'SL',
  ['']   = 'Sb',
  ['i']    = 'In',
  ['ic']   = 'In',
  ['ix']   = 'In',
  ['R']    = 'Re',
  ['Rc']   = 'Re',
  ['Rv']   = 'V-REPLACE',
  ['Rx']   = 'Re',
  ['c']    = 'Cm',
  ['cv']   = 'EX',
  ['ce']   = 'EX',
  ['r']    = 'Re',
  ['rm']   = 'MORE',
  ['r?']   = 'CONFIRM',
  ['!']    = 'Sh',
  ['t']    = 'Tr',
}

function M.mode()
    local mode_code = vim.api.nvim_get_mode().mode
    if M.map[mode_code] == nil then return mode_code end
    return M.map[mode_code]
end

return M
