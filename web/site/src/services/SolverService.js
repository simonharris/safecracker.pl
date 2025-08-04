import axios from 'axios';


//const API_HOST = 'http://192.168.1.73:80'
const API_HOST = 'https://safe-api.pointbeing.net'


class SolverService {
    constructor() {
        this.eventSource = null;
        this.listeners = [];
        this.messages = [];
    }

    addListener(listener) {
        this.listeners.push(listener);
    }

//   removeListener(listener) {
//     const index = this.listeners.indexOf(listener);
//     if (index !== -1) {
//       this.listeners.splice(index, 1);
//     }
//   }
    solveExample(puzzleid) {
        const path = '/solve/example/' + puzzleid;
        this.solve(path);
    }

    solveUpload(filename) {
        const path = '/solve/upload/' + filename;
        this.solve(path);
    }

    solve(path) {

        this.eventSource = new EventSource(API_HOST + path);

        this.eventSource.onerror = () => {
            console.log('Error occurred. Ready state:', this.eventSource.readyState);
        };
        this.eventSource.onopen = () => {
            console.log('Connection established');
        };
        this.eventSource.addEventListener('begin', (event) => {
            console.log('Received "begin" event:', event.data);
            this.messages = [];
            //this.messages.push(event);
        });
        this.eventSource.addEventListener('end', (event) => {
            console.log('Received "end" event:', event.data);
            this.eventSource.close();
            this.messages.push(event);
            //console.log(this.messages);
        });
        this.eventSource.addEventListener('message', (event) => {
            console.log('Received "message" event:', JSON.parse(event.data));
            this.messages.push(JSON.parse(event.data));
        });
        this.eventSource.addEventListener('update', (event) => {
            console.log('Received "update" event:', JSON.parse(event.data));
            this.messages[this.messages.length - 1] = JSON.parse(event.data);
        });
        this.eventSource.addEventListener('solution', (event) => {
            console.log('Received "solution" event:', event);
        });
    }

    async getExamples() {
        try {
            const response = await fetch(API_HOST + '/examples');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const data = await response.json();
            return data;
        } catch (error) {
            console.error(error);
            throw error;
        }
    }

    async upload(file) {

        const formData = new FormData();
        formData.append('file', file);

        this.uploading = true;
        return axios.post(API_HOST + '/upload', formData, {
            onUploadProgress: (progressEvent) => {
                this.progress = Math.round((progressEvent.loaded * 100) / progressEvent.total);
            }
        })
        // .then((response) => {
        //     this.uploading = false;
        //     console.log('From service: ' + response.data)
        // })
        // .catch((error) => {
        //     console.error(error);
        //     this.uploading = false;
        // })
    }
}

export default new SolverService();
