document.addEventListener('DOMContentLoaded', function() {
    // Cria o seletor de idioma
    const languageSwitcher = document.createElement('div');
    languageSwitcher.className = 'language-switcher';

    const select = document.createElement('select');
    select.id = 'language-select';

    const options = [
        { value: 'pt', text: 'Português' },
        { value: 'en', text: 'English' }
    ];

    options.forEach(option => {
        const opt = document.createElement('option');
        opt.value = option.value;
        opt.textContent = option.text;
        select.appendChild(opt);
    });

    // Adiciona o evento de mudança de idioma
    select.addEventListener('change', function() {
        const lang = this.value;
        translatePage(lang);
    });

    languageSwitcher.appendChild(select);

    // Adiciona o seletor à barra de navegação
    const navbar = document.querySelector('.navbar-collapse'); // Ajuste o seletor conforme necessário
    if (navbar) {
        navbar.appendChild(languageSwitcher);
    }

    // Função para traduzir a página
    function translatePage(targetLang) {
        const elementsToTranslate = document.querySelectorAll('p, h1, h2, h3, h4, h5, h6, li, a, span'); // Elementos a serem traduzidos
        const sourceLang = targetLang === 'en' ? 'pt' : 'en'; // Idioma original

        elementsToTranslate.forEach(element => {
            const text = element.innerText;

            fetch('https://libretranslate.com/translate', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    q: text,
                    source: sourceLang,
                    target: targetLang,
                    format: 'text'
                })
            })
            .then(response => {
                if (!response.ok) {
                    throw new Error('Erro na requisição');
                }
                return response.json();
            })
            .then(data => {
                console.log('Resposta da API:', data); // Depuração: exibe a resposta da API no console
                if (data.translatedText) {
                    element.innerText = data.translatedText;
                } else {
                    console.error('Texto traduzido não encontrado na resposta:', data);
                }
            })
            .catch(error => {
                console.error('Erro ao traduzir:', error);
            });
        });
    }
});

fetch('https://libretranslate.com/translate', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
        q: text,
        source: sourceLang,
        target: targetLang,
        format: 'text'
    }),
    mode: 'cors' // Adiciona o modo CORS
})