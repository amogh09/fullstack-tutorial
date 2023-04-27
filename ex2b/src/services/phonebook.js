import axios from 'axios'

const baseUrl = '/api/persons'

const create = (person) => {
  return axios.post(baseUrl, person).then(res => res.data)
}

const getAll = () => {
  return axios.get(baseUrl).then(res => res.data)
}

const del = (id) => {
  return axios.delete(`${baseUrl}/${id}`)
}

const update = (id, person) => {
  return axios.put(`${baseUrl}/${id}`, person).then(res => res.data)
}

export default { create, getAll, del, update }
