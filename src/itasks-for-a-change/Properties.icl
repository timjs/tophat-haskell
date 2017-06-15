implementation module Properties

import StdString

:: Properties = { user :: String, label :: String }

instance property User where
  getProperty ps = User ps.user
  setProperty (User p) ps = { ps & user = p }

instance property Label where
  getProperty ps = Label ps.label
  setProperty (Label p) ps = { ps & label = p }

mkProperties :: Properties
mkProperties = { user = "", label = "" }

instance toString Properties where
  toString ps = "label = " +++ ps.label +++ "; user = " +++ ps.user
