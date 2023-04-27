import axios from 'axios'

const baseUrl = '/api/notes'

const getAll = () => {
  const nonExisting = {
    id: 10000,
    content: 'This note is not saved to server',
    important: true,
  }
  return axios.get(baseUrl).then(res => res.data.concat(nonExisting))
}

const create = (note) => {
  return axios.post(baseUrl, note).then(res => res.data)
}

const update = (id, note) => {
  return axios.put(`${baseUrl}/${id}`, note).then(res => res.data)
}

const exp = { getAll, create, update }
export default exp
