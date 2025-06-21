<script setup>
import solverService from '@/services/SolverService.js';
</script>

<template>

<form @submit.prevent="handleSubmit">

<select v-model="selectedExample">
<option value="" selected>Please select example &raquo;</option>
    <option v-for="example in examples" :key="example.id" :value="example.id">
    {{ example.name }}
    </option>
</select>

<button>Solve</button>

</form>

</template>

<script>

export default {
  name: 'ExampleSelectForm',
  data() {
    return {
      examples: [],
      selectedExample: "",
      service: solverService,
    }
  },
  mounted() {
    this.fetchExamples();
  },
  methods: {
    async fetchExamples() {
      try {
        const data = await this.service.getExamples();
        this.examples = data.examples;
      } catch (error) {
        console.error(error);
      }
    },
    handleSubmit() {
        this.$emit('form-submitted', this.selectedExample);
    }
  },
}

</script>