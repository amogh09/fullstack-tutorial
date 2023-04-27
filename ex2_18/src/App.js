import { useState, useEffect } from 'react'
import axios from 'axios'

const Finder = ({ filter, handleFilterChange }) =>
  <div>
    find countries <input value={filter} onChange={handleFilterChange} />
  </div>

const CountryName = ({ country, setShowCountry }) =>
  <div>
    {country.name.common} <button onClick={() => setShowCountry(country)}>Show</button>
  </div>

const Country = ({ country }) =>
  <div>
    <h2>{country.name.common}</h2>
    <div>
      capital {country.capital[0]}
      <br />
      area {country.area}
    </div>
    <div>
      <h3>Languages</h3>
      <ul>
        {Object.values(country.languages).map(l => <li key={l}>{l}</li>)}
      </ul>
    </div>
    <div style={{ fontSize: 200 }} >
      {country.flag}
    </div>
  </div >

const Result = ({ countries, showCountry, setShowCountry }) => {
  if (countries.length === 0) {
    return null
  }

  const first =
    countries.length > 10
      ? (<p>Too many matches, specify another filter</p>)
      : countries.length === 1
        ? (<Country country={countries[0]} />)
        : <div>
          {countries.map(c =>
            <CountryName
              key={c.name.official}
              country={c}
              setShowCountry={setShowCountry}
            />)}
        </div>
  const second = showCountry === null ? null : <Country country={showCountry} />

  return (
    <div>
      {first}
      {second}
    </div>
  )
}

const App = () => {
  const [filter, setFilter] = useState('')
  const [countries, setCountries] = useState([])
  const [filteredCountries, setFilteredCountries] = useState([])
  const [showCountry, setShowCountry] = useState(null)

  const handleFilterChange = (event) => {
    const newFilter = event.target.value
    setFilter(newFilter)
    setFilteredCountries(countries.filter(c =>
      c.name.common.toLowerCase().startsWith(newFilter.toLowerCase())))
    setShowCountry(null)
  }

  useEffect(() => {
    axios.get('https://restcountries.com/v3.1/all')
      .then(res => {
        setCountries(res.data)
        setFilteredCountries(res.data)
      })
  }, [])

  return (
    <div>
      <Finder filter={filter} handleFilterChange={handleFilterChange} />
      <Result
        countries={filteredCountries}
        showCountry={showCountry}
        setShowCountry={setShowCountry}
      />
    </div>
  );
}

export default App;
