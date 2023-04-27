import { useState } from 'react'
import PropTypes from 'prop-types'

const Toggable = (props) => {
  const [visible, setVisible] = useState(false)

  const hideWhenVisible = { display: visible ? 'none' : '' }
  const showWhenVisible = { display: visible ? '' : 'none' }

  const toggleVisibility = () => { setVisible(!visible) }

  return (
    <div>
      <div style={hideWhenVisible}>
        <button onClick={toggleVisibility}>{props.buttonLabel}</button>
      </div>
      <div style={showWhenVisible} className="toggableContent">
        {props.children}
        <button onClick={toggleVisibility}>Cancel</button>
      </div>
    </div>
  )
}

Toggable.propTypes = {
  buttonLabel: PropTypes.string.isRequired
}

export default Toggable
