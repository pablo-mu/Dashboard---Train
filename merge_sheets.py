import pandas as pd
import os

# Ruta del archivo Excel
file_path = '.data/lineas_actividad.xlsx'
output_path = '.data/linea_actividad_unificada.xlsx'

def merge_excel_sheets():
    # Verificar si el archivo existe
    if not os.path.exists(file_path):
        print(f"Error: El archivo {file_path} no existe.")
        return

    try:
        # Leer las hojas "RIBERA" y "FE"
        print("Leyendo hoja 'RIBERA'...")
        df_ribera = pd.read_excel(file_path, sheet_name='Ribera')
        
        print("Leyendo hoja 'FE'...")
        df_fe = pd.read_excel(file_path, sheet_name='Fe')

        # Concatenar ambos DataFrames
        print("Uniendo datos...")
        df_combined = pd.concat([df_ribera, df_fe], ignore_index=True)

        # Eliminar duplicados basándose en todas las columnas (CAC, Línea de Actividad, Modalidad)
        # Si hay otras columnas, esto también considerará esas para la duplicidad.
        # Si solo se deben considerar esas 3 columnas específicas, se puede usar subset=['CAC', 'Línea de Actividad', 'Modalidad']
        print("Eliminando duplicados...")
        df_unique = df_combined.drop_duplicates()

        # Ordenar por CAC para mantener el orden lógico
        print("Ordenando datos por CAC...")
        # Usamos una columna temporal para ordenar como string sin modificar los datos originales
        df_unique['_sort_key'] = df_unique['CAC'].astype(str)
        df_unique = df_unique.sort_values(by=['_sort_key'])
        df_unique = df_unique.drop(columns=['_sort_key'])

        # Comprobar si algún CAC aparece más de una vez
        print("Verificando duplicados de CAC...")
        cac_counts = df_unique['CAC'].value_counts()
        duplicated_cacs = cac_counts[cac_counts > 1]
        
        if not duplicated_cacs.empty:
            print("¡Atención! Los siguientes CAC aparecen más de una vez (tienen múltiples combinaciones):")
            print(duplicated_cacs)
        else:
            print("Todos los CAC son únicos (una sola combinación por CAC).")

        # Guardar el resultado en un nuevo archivo Excel
        print(f"Guardando resultado en {output_path}...")
        df_unique.to_excel(output_path, index=False)
        
        print("¡Proceso completado con éxito!")
        print(f"Filas totales antes de eliminar duplicados: {len(df_combined)}")
        print(f"Filas totales después de eliminar duplicados: {len(df_unique)}")

    except Exception as e:
        print(f"Ocurrió un error: {e}")

if __name__ == "__main__":
    merge_excel_sheets()
