import '@testing-library/jest-dom/extend-expect'
import userEvent from '@testing-library/user-event'
import { render, screen } from '@testing-library/react'
import Toggable from './Toggable'

describe('<Toggable />', () => {
  let container

  beforeEach(() => {
    container = render(
      <Toggable buttonLabel="show...">
        <div className="testDiv">
          toggable content
        </div>
      </Toggable>
    ).container
  })

  test('renders its children', async () => {
    await screen.findAllByText('toggable content')
  })

  test('at start the children are not visible', () => {
    const div = container.querySelector('.toggableContent')
    expect(div).toHaveStyle('display: none')
  })

  test('after clicking the button, children are displayed', async () => {
    const user = userEvent.setup()
    const button = screen.getByText('show...')
    await user.click(button)

    const div = container.querySelector('.toggableContent')
    expect(div).not.toHaveStyle('display: none')
  })

  test('toggable content can be closed', async () => {
    const user = userEvent.setup()
    const button = screen.getByText('show...')
    await user.click(button)

    const cancelButton = screen.getByText('Cancel')
    await user.click(cancelButton)

    const div = container.querySelector('.toggableContent')
    expect(div).toHaveStyle('display: none')
  })
})
