definition module Properties

import StdString

:: Properties

class property p where
  setProperty :: p Properties -> Properties
  getProperty :: Properties -> p

:: User = User String
:: Label = Label String

instance property User, Label

mkProperties :: Properties

instance toString Properties
