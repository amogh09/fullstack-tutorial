import { useState } from 'react'

const Button = ({ handleClick, text }) =>
  <button onClick={handleClick}>{text}</button>

const Feedback = ({ incrementGood, incrementNeutral, incrementBad }) =>
  <div>
    <h1>give feedback</h1>
    <Button handleClick={() => incrementGood()} text="good" />
    <Button handleClick={() => incrementNeutral()} text="neutral" />
    <Button handleClick={() => incrementBad()} text="bad" />
  </div>

const StatLine = ({ text, value, isPercent }) =>
  <tr>
    <td>{text}</td>
    <td>{value} {isPercent ? "%" : ""}</td>
  </tr>

const Stats = ({ good, neutral, bad }) =>
  good + neutral + bad === 0
    ? <div>
      <h1>statistics</h1>
      <div>No feedback given</div>
    </div>
    :
    <div>
      <h1>statistics</h1>
      <table>
        <tbody>
          <StatLine text='good' value={good} />
          <StatLine text='neutral' value={neutral} />
          <StatLine text='bad' value={bad} />
          <StatLine text='average' value={average(good, neutral, bad)} />
          <StatLine text='positivePercent'
            value={positivePercent(good, neutral, bad)} isPercent={true} />
        </tbody>
      </table>
    </div>

const average = (good, neutral, bad) =>
  (good - bad) / (good + neutral + bad)

const positivePercent = (good, neutral, bad) =>
  good / (good + neutral + bad) * 100

const increment = (value, setter) => () => setter(value + 1)

const App = () => {
  // save clicks of each button to its own state
  const [good, setGood] = useState(0)
  const [neutral, setNeutral] = useState(0)
  const [bad, setBad] = useState(0)

  return (
    <div>
      <Feedback
        incrementGood={increment(good, setGood)}
        incrementNeutral={increment(neutral, setNeutral)}
        incrementBad={increment(bad, setBad)}
      />
      <Stats good={good} neutral={neutral} bad={bad} />
    </div>
  )
}

export default App
