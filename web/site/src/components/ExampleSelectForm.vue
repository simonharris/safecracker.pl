<script setup>
import solverService from '@/services/SolverService.js';
</script>

<template>
<!-- div class="form-group d-flex flex-wrap justify-content-center" -->
<form @submit.prevent="handleSubmit">

<select v-model="selectedExample" class="form-control w-100 Xw-lg-50 mb-2">
<option value="" selected>Please select example &raquo;</option>
    <option v-for="example in examples" :key="example.id" :value="example.id">
    {{ example.name }}
    </option>
</select>

<button class="btn btn-secondary w-100 Xw-lg-50">Solve</button>

</form>
<!-- /div -->
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