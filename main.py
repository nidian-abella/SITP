# Run this app with `python app.py` and
# visit http://127.0.0.1:8050/ in your web browser.

from dash import Dash, html, dcc
from dash.dependencies import Input, Output
from plotly.subplots import make_subplots

import os
os.environ["OMP_NUM_THREADS"] = '1'

import pandas as pd
from sklearn.metrics import silhouette_score
from sklearn.cluster import KMeans

import plotly.graph_objs as go
import plotly.express as px

app = Dash(__name__)
server = app.server

colors = {
    'background': '#111111',
    'text': '#061fc4'
}

data = pd.read_csv("BaseSITP.csv")
data.index = pd.to_datetime(data["Año"].astype(str) + "-" + data["Mes"].astype(str))
data.index = pd.to_datetime(data.index)
serie_temp = data[["ICO", "ICK"]]
data_scaled = serie_temp.values


# Método del codo
inertias = []
ks = list(range(1, 11))

for k in ks:
    kmeans = KMeans(n_clusters=k, n_init=10)
    kmeans.fit(data_scaled)
    inertias.append(kmeans.inertia_)

# Índice de Silhouette
sil_scores = []

for k in range(2, 11):
    kmeans = KMeans(n_clusters=k, n_init=10)
    kmeans.fit(data_scaled)
    labels = kmeans.labels_
    sil_score = silhouette_score(data_scaled, labels)
    sil_scores.append(sil_score)

# Calcular la media y la desviación estándar de los datos agrupados por fecha
grouped_data = serie_temp[["ICO", "ICK"]].groupby(serie_temp.index.date).agg(['mean', 'std'])
grouped_data.index = pd.to_datetime(grouped_data.index)

# Crear las bandas superior e inferior para cada serie
grouped_data['ICO', 'upper'] = grouped_data['ICO', 'mean'] + grouped_data['ICO', 'std']
grouped_data['ICO', 'lower'] = grouped_data['ICO', 'mean'] - grouped_data['ICO', 'std']
grouped_data['ICK', 'upper'] = grouped_data['ICK', 'mean'] + grouped_data['ICK', 'std']
grouped_data['ICK', 'lower'] = grouped_data['ICK', 'mean'] - grouped_data['ICK', 'std']


app.layout = html.Div([
    html.H1(
        children='SITP Data - Kmeans clusters applid for ICO and ICK indicators',
        style={
            'textAlign': 'center',
            'color': colors['text']
        }
    ),
    html.Div(children=[
        html.Label('Graficas con info inicial', style={"font-weight": "bold", "font-size":"28px"}),
        dcc.Dropdown(
            ['Serie de tiempo', 'Elbow KMeans', 'Indice de Silhouette'], 
            'Serie de tiempo', 
            id="grafica-tipo-inicial",
            style={"margin-top": 16, "margin-bottom": 32}
            ),
        dcc.Graph(
            id='sitp-data-ico-vs-ick',
            figure=go.Figure(),
        ),
        html.Div([
            dcc.Slider(
                step=None,
                marks={
                    0: {"label":"Periodo completo", "style":{"white-space": "pre-line", "font-size": "1.4em"}} ,
                    1: {"label":"Pre pandemia\n(Ene 2019 - Feb 2020)", "style":{"white-space": "pre-line", "font-size": "1.4em"}} ,
                    2: {"label":"Restricciones en pandemia\n(Mar 2020 - Nov 2020)", "style":{"white-space": "pre-line", "font-size": "1.4em"}},
                    3: {"label":"Post Pandemia\n(Dic 2020 - Jun 2021)", "style":{"white-space": "pre", "font-size": "1.4em"}}
                },
                id='year-slider',
            )
        ], id="slider-wrapper",
            style={"display": "block"})
    ], style={"padding": 50}),


    html.Div(children=[
        html.Label('Graficas KMeans resultados', style={"font-weight": "bold", "font-size":"28px"}),
        dcc.Dropdown(
            ['Clusters vs ICO', 'Clusters vs ICK', 'Dispersion de clusters por ICO y ICK'], 
            'Dispersion de clusters por ICO y ICK', 
            id="grafica-kmeans",
            style={"margin-top": 16}
            ),
        html.Br(),
        html.Div([
                "K-Clusters: ",
                dcc.Input(id='k-number', value=3, type='number', max=10, min=1, style={"height":30, "margin-left":10})
            ], style={"margin-top":30, "margin-bottom":30}),
        dcc.Graph(
            id='k-means-results',
            figure=go.Figure()
        )
        ], style={"padding": 50}),
    
])


@app.callback(
    Output('slider-wrapper', 'style'),
    Input('grafica-tipo-inicial', 'value'),
)
def visibility_year_slider(grafica_tipo_inicial):
    if grafica_tipo_inicial == 'Serie de tiempo':
        return {'display': 'block'}
    else:
        return {'display': 'none'}



@app.callback(
    Output('sitp-data-ico-vs-ick', 'figure'),
    Input('grafica-tipo-inicial', 'value'),
    Input('year-slider', 'value')
)
def sitp_data_ico_vs_ick(grafica_tipo_inicial, year_slider_value):
    fig = go.Figure()

    if grafica_tipo_inicial == 'Serie de tiempo':
        if (year_slider_value == None or year_slider_value == 0):
            fig.add_trace(go.Scatter(x=grouped_data.index, y=grouped_data['ICO', 'upper'], line=dict(width=0), showlegend=False, mode='lines', name='ICO_upper', fill=None))
            fig.add_trace(go.Scatter(x=grouped_data.index, y=grouped_data['ICO', 'lower'], line=dict(width=0), showlegend=False, mode='lines', name='ICO_lower', fill='tonexty'))
            fig.add_trace(go.Scatter(x=grouped_data.index, y=grouped_data['ICO', 'mean'], mode='lines', name='ICO', line=dict(width=2), marker=dict(color="blue")))

            fig.add_trace(go.Scatter(x=grouped_data.index, y=grouped_data['ICK', 'upper'], line=dict(width=0), showlegend=False, mode='lines', name='ICK_upper', fill=None))
            fig.add_trace(go.Scatter(x=grouped_data.index, y=grouped_data['ICK', 'lower'], line=dict(width=0), showlegend=False, mode='lines', name='ICK_lower', fill='tonexty'))
            fig.add_trace(go.Scatter(x=grouped_data.index, y=grouped_data['ICK', 'mean'], mode='lines', name='ICK', line=dict(width=2), marker=dict(color="red")))

            fig.update_layout(title='Series Temporales ICO e ICK con Bandas', xaxis_title='Tiempo', yaxis_title='Valores', legend_title='Series', height=500)

        elif (year_slider_value == 1):
            grouped_data_filtered = grouped_data.loc["2019-1-1": "2020-2-29"]
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'upper'], line=dict(width=0), showlegend=False, mode='lines', name='ICO_upper', fill=None))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'lower'], line=dict(width=0), showlegend=False, mode='lines', name='ICO_lower', fill='tonexty'))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'mean'], mode='lines', name='ICO', line=dict(width=2), marker=dict(color="blue")))

            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'upper'], line=dict(width=0), showlegend=False, mode='lines', name='ICK_upper', fill=None))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'lower'], line=dict(width=0), showlegend=False, mode='lines', name='ICK_lower', fill='tonexty'))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'mean'], mode='lines', name='ICK', line=dict(width=2), marker=dict(color="red")))

            fig.update_layout(title='Series Temporales ICO e ICK con Bandas', xaxis_title='Tiempo', yaxis_title='Valores', legend_title='Series', height=500)
        
        elif (year_slider_value == 2):
            grouped_data_filtered = grouped_data.loc["2020-3-1": "2020-11-30"]
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'upper'], line=dict(width=0), showlegend=False, mode='lines', name='ICO_upper', fill=None))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'lower'], line=dict(width=0), showlegend=False, mode='lines', name='ICO_lower', fill='tonexty'))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'mean'], mode='lines', name='ICO', line=dict(width=2), marker=dict(color="blue")))

            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'upper'], line=dict(width=0), showlegend=False, mode='lines', name='ICK_upper', fill=None))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'lower'], line=dict(width=0), showlegend=False, mode='lines', name='ICK_lower', fill='tonexty'))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'mean'], mode='lines', name='ICK', line=dict(width=2), marker=dict(color="red")))

            fig.update_layout(title='Series Temporales ICO e ICK con Bandas', xaxis_title='Tiempo', yaxis_title='Valores', legend_title='Series', height=500)
        
        elif (year_slider_value == 3):
            grouped_data_filtered = grouped_data.loc["2020-12-1": "2021-7-1"]
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'upper'], line=dict(width=0), showlegend=False, mode='lines', name='ICO_upper', fill=None))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'lower'], line=dict(width=0), showlegend=False, mode='lines', name='ICO_lower', fill='tonexty'))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICO', 'mean'], mode='lines', name='ICO', line=dict(width=2), marker=dict(color="blue")))

            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'upper'], line=dict(width=0), showlegend=False, mode='lines', name='ICK_upper', fill=None))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'lower'], line=dict(width=0), showlegend=False, mode='lines', name='ICK_lower', fill='tonexty'))
            fig.add_trace(go.Scatter(x=grouped_data_filtered.index, y=grouped_data_filtered['ICK', 'mean'], mode='lines', name='ICK', line=dict(width=2), marker=dict(color="red")))

            fig.update_layout(title='Series Temporales ICO e ICK con Bandas', xaxis_title='Tiempo', yaxis_title='Valores', legend_title='Series', height=500)


    elif grafica_tipo_inicial == 'Indice de Silhouette':
        fig.add_trace(
            go.Scatter(x=list(range(2, 11)), y=sil_scores, mode='lines+markers', name='Índice de Silhouette'),
        )

        fig.update_layout(
            title="Índice de Silhouette vs Número de clusters (K)",
            xaxis_title="Número de clusters (K)",
            yaxis_title="Índice de Silhouette", height=500
        )

    elif grafica_tipo_inicial == 'Elbow KMeans':
        fig.add_trace(
            go.Scatter(x=list(range(2, 11)), y=inertias, mode='lines+markers', name='Inercia'),
        )

        fig.update_layout(
            title="Inercia vs Número de clusters (K)",
            xaxis_title="Número de clusters (K)",
            yaxis_title="Inercia", height=500
        )

    fig.update_xaxes(tickfont=dict(family='Arial, bold', color='black', size=16), title_font=dict(size=16, family='Arial, bold', color='black'))
    fig.update_yaxes(tickfont=dict(family='Arial, bold', color='black', size=16), title_font=dict(size=16, family='Arial, bold', color='black'))

    return fig



@app.callback(
    Output('k-means-results', 'figure'),
    Input('grafica-kmeans', 'value'),
    Input('k-number', 'value'),
)
def k_means_graph(grafica_kmeans, k_number):
    colors = ["aliceblue", "aqua", "aquamarine",
         "bisque",  "blue","lemonchiffon",
        "blueviolet", "brown", "sienna", "cadetblue"]

    if k_number is None:
        return go.Figure()

    # Ejemplo con K=K_elegidos (sustituir por el valor óptimo encontrado)
    kmeans = KMeans(n_clusters=k_number, n_init=10)
    kmeans.fit(data_scaled)
    cluster_labels = kmeans.labels_
    serie_temp['Cluster'] = cluster_labels
    serie_temp['Cluster'] = serie_temp['Cluster'].map({
        0:"G1",
        1:"G2",
        2:"G3",
        3:"G4",
        4:"G5",
        5:"G6",
        6:"G7",
        7:"G8",
        8:"G9",
        9:"G10",
        10:"G11",
    })

    # Agrupamos los datos por Cluster y calculamos el promedio de ICO
    grouped_data_ICO = serie_temp.groupby('Cluster')['ICO'].mean().reset_index()
    # Agrupamos los datos por Cluster y calculamos el promedio de ICK
    grouped_data_ICK = serie_temp.groupby('Cluster')['ICK'].mean().reset_index()

    if grafica_kmeans == 'Dispersion de clusters por ICO y ICK':
        centroides = kmeans.cluster_centers_
        centroides = pd.DataFrame(centroides, columns=["ICO", "ICK"])
        centroides["Cluster"] = [f"G{i}" for i in range(1, len(kmeans.cluster_centers_) + 1)]
        centroides.sort_values(by="Cluster", key=lambda label: label.str[1:].astype(int), inplace=True)

        cluster_names = serie_temp['Cluster'].unique().tolist()
        cluster_names = list(sorted(cluster_names, key=lambda label: int(label[1:])))

        scatters = make_subplots()

        for i, cluster_name in enumerate(cluster_names):
            serie_temp_filtrada = serie_temp.loc[serie_temp["Cluster"] == cluster_name]

            # Crear un scatter plot para los datos
            scatters.add_trace(go.Scatter(
                x=serie_temp_filtrada['ICO'],
                y=serie_temp_filtrada['ICK'],
                mode='markers',
                marker=dict(
                    color=colors[i],  # set color equal to a variable
                    # colorscale='Viridis',  # one of plotly colorscales
                    size=10,
                    line_width=1),
                text=serie_temp_filtrada['Cluster'],  # hover text
                hoverinfo='text',
                name=cluster_name
            ))

        # Crear un scatter plot para los centroides
        scatters.add_trace(go.Scatter(
            x=centroides['ICO'],
            y=centroides['ICK'],
            mode='markers',
            marker=dict(
                color=colors[:k_number],
                size=80,
                opacity=0.5,
                line_width=2),
            name='Centroides',
            text=centroides['Cluster'],
            hoverinfo='text'
            
        ))

        # data_plot = [scatters, centroid_scatter]

        layout = go.Layout(
            title='Scatter plot of ICO vs ICK',
            xaxis=dict(title='ICO'),
            yaxis=dict(title='ICK'),
            showlegend=True
        )

        fig = go.Figure(data=scatters, layout=layout)
        fig.update_layout(title='Dispersion de clusters por ICO y ICK', xaxis_title='ICO', yaxis_title='ICK', legend_title='Graficos', height=500)


    elif (grafica_kmeans == "Clusters vs ICO"):
        
        # Creamos el gráfico
        fig = go.Figure(data=go.Bar(
                    x=grouped_data_ICO['Cluster'],
                    y=grouped_data_ICO['ICO'],
                    textposition='auto',
                    text=grouped_data_ICO['ICO'].round(2),
                    marker_color=colors[:k_number],
                    textfont=dict(
                        family="Arial, bold",
                        size=16,
                        color="Black",
                    ),
                    name="Barras"
                ))

        fig.update_layout(title='Clusters vs ICO', xaxis_title='Cluster', yaxis_title='ICO', legend_title='Barras', height=500)
    
    elif (grafica_kmeans == "Clusters vs ICK"):
        # Creamos el gráfico
        fig = go.Figure(data=go.Bar(
                    x=grouped_data_ICK['Cluster'],
                    y=grouped_data_ICK['ICK'],
                    textposition='auto',
                    text=grouped_data_ICK['ICK'].round(2),
                    marker_color=colors[:k_number],
                    textfont=dict(
                        family="Arial, bold",
                        size=16,
                        color="Black",
                    ),
                ))

        fig.update_layout(title='Clusters vs ICK', xaxis_title='Cluster', yaxis_title='ICK', legend_title='Graficos', height=500)
    
    fig.update_xaxes(tickfont=dict(family='Arial, bold', color='black', size=16), title_font=dict(size=16, family='Arial, bold', color='black'))
    fig.update_yaxes(tickfont=dict(family='Arial, bold', color='black', size=16), title_font=dict(size=16, family='Arial, bold', color='black'))
    return fig



app.run_server(host= '0.0.0.0',debug=False)