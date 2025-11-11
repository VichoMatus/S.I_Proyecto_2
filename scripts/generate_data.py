#!/usr/bin/env python3
"""
Generador de archivo data.dat para el Proyecto #2 - Sistemas Inteligentes
Genera registros binarios compatibles con la estructura Pascal TRegistro

Estructura Pascal:
TRegistro = Record
  id : Byte;              // ID de estación (1-10)
  te : Byte;              // Temperatura (°C)
  hr : Byte;              // Humedad del aire (%)
  mp01: Word;             // Material Particulado 0.1 um
  mp25: Word;             // Material Particulado 2.5 um
  mp10: Word;             // Material Particulado 10 um
  h01: Word;              // Histograma de partículas 0.1
  h25: Word;              // Histograma de partículas 2.5
  h50: Word;              // Histograma de partículas 5.0
  h10: Word;              // Histograma de partículas 10
end;

Tamaño total: 1 + 1 + 1 + 2 + 2 + 2 + 2 + 2 + 2 + 2 = 17 bytes
"""

import struct
import random
import argparse
from pathlib import Path
from typing import List, Tuple


class RegistroAmbiente:
    """Representa un registro de datos ambientales"""
    
    def __init__(self, estacion_id: int):
        """
        Genera un registro con datos aleatorios realistas
        
        Args:
            estacion_id: ID de la estación (1-10)
        """
        self.id = estacion_id
        
        # Temperatura: 15-35°C
        self.te = random.randint(15, 35)
        
        # Humedad: 30-80%
        self.hr = random.randint(30, 80)
        
        # Material Particulado (valores típicos en µg/m³)
        # PM0.1: 0-100
        self.mp01 = random.randint(0, 100)
        
        # PM2.5: 0-150 (valores más altos = mala calidad del aire)
        self.mp25 = random.randint(0, 150)
        
        # PM10: 0-200
        self.mp10 = random.randint(0, 200)
        
        # Histogramas de partículas (conteos por rango de tamaño)
        # Valores típicos: 100-5000 partículas
        self.h01 = random.randint(1000, 5000)  # Partículas > 0.1 um
        self.h25 = random.randint(500, 3000)   # Partículas > 2.5 um
        self.h50 = random.randint(200, 1500)   # Partículas > 5.0 um
        self.h10 = random.randint(50, 800)     # Partículas > 10 um
    
    def to_bytes(self) -> bytes:
        """
        Convierte el registro a formato binario compatible con Pascal
        Formato: little-endian
        B = unsigned char (1 byte)
        H = unsigned short (2 bytes, little-endian)
        
        Returns:
            bytes: 17 bytes del registro
        """
        return struct.pack(
            '<BBBHHHHHHH',  # < = little-endian, B = byte, H = word (2 bytes)
            self.id,
            self.te,
            self.hr,
            self.mp01,
            self.mp25,
            self.mp10,
            self.h01,
            self.h25,
            self.h50,
            self.h10
        )
    
    def __str__(self) -> str:
        """Representación en texto del registro"""
        return (
            f"Estación {self.id}: "
            f"Temp={self.te}°C, HR={self.hr}%, "
            f"PM0.1={self.mp01}, PM2.5={self.mp25}, PM10={self.mp10}, "
            f"Hist=[{self.h01}, {self.h25}, {self.h50}, {self.h10}]"
        )


def generar_registros(num_registros: int, num_estaciones: int = 10) -> List[RegistroAmbiente]:
    """
    Genera una lista de registros de datos ambientales
    
    Args:
        num_registros: Número total de registros a generar
        num_estaciones: Número de estaciones (default: 10)
    
    Returns:
        Lista de registros generados
    """
    registros = []
    
    # Distribuir registros entre estaciones
    registros_por_estacion = num_registros // num_estaciones
    registros_extra = num_registros % num_estaciones
    
    for estacion_id in range(1, num_estaciones + 1):
        # Calcular cuántos registros para esta estación
        count = registros_por_estacion
        if estacion_id <= registros_extra:
            count += 1
        
        # Generar registros para esta estación
        for _ in range(count):
            registros.append(RegistroAmbiente(estacion_id))
    
    # Mezclar para simular llegada aleatoria de datos
    random.shuffle(registros)
    
    return registros


def escribir_archivo_binario(registros: List[RegistroAmbiente], archivo_salida: Path) -> None:
    """
    Escribe los registros en formato binario compatible con Pascal
    
    Args:
        registros: Lista de registros a escribir
        archivo_salida: Path del archivo de salida
    """
    # Crear directorio si no existe
    archivo_salida.parent.mkdir(parents=True, exist_ok=True)
    
    # Escribir archivo binario
    with open(archivo_salida, 'wb') as f:
        for registro in registros:
            f.write(registro.to_bytes())
    
    print(f"✅ Archivo generado: {archivo_salida}")
    print(f"   Total de registros: {len(registros)}")
    print(f"   Tamaño del archivo: {archivo_salida.stat().st_size} bytes")
    print(f"   Tamaño por registro: 17 bytes")


def mostrar_muestra(registros: List[RegistroAmbiente], cantidad: int = 5) -> None:
    """
    Muestra una muestra de los registros generados
    
    Args:
        registros: Lista de registros
        cantidad: Número de registros a mostrar
    """
    print(f"\n📊 Muestra de registros generados (primeros {cantidad}):")
    print("-" * 80)
    
    for i, registro in enumerate(registros[:cantidad], 1):
        print(f"{i:3d}. {registro}")
    
    if len(registros) > cantidad:
        print(f"     ... ({len(registros) - cantidad} registros más)")


def main():
    """Función principal"""
    parser = argparse.ArgumentParser(
        description='Generador de archivo data.dat para Proyecto #2',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Ejemplos de uso:
  python generate_data.py --count 100
  python generate_data.py --count 1000 --output ../data/data.dat
  python generate_data.py -c 500 -s 5
        """
    )
    
    parser.add_argument(
        '-c', '--count',
        type=int,
        default=100,
        help='Número total de registros a generar (default: 100)'
    )
    
    parser.add_argument(
        '-s', '--stations',
        type=int,
        default=10,
        help='Número de estaciones (default: 10)'
    )
    
    parser.add_argument(
        '-o', '--output',
        type=str,
        default='../data/data.dat',
        help='Archivo de salida (default: ../data/data.dat)'
    )
    
    parser.add_argument(
        '--seed',
        type=int,
        default=None,
        help='Semilla para generación aleatoria (para reproducibilidad)'
    )
    
    parser.add_argument(
        '--no-sample',
        action='store_true',
        help='No mostrar muestra de registros'
    )
    
    args = parser.parse_args()
    
    # Configurar semilla si se especificó
    if args.seed is not None:
        random.seed(args.seed)
        print(f"🌱 Usando semilla: {args.seed}")
    
    # Validar argumentos
    if args.count < 1:
        print("❌ Error: --count debe ser al menos 1")
        return 1
    
    if args.stations < 1 or args.stations > 255:
        print("❌ Error: --stations debe estar entre 1 y 255")
        return 1
    
    print(f"\n🔧 Generando archivo de datos...")
    print(f"   Registros: {args.count}")
    print(f"   Estaciones: {args.stations}")
    print(f"   Archivo: {args.output}")
    
    # Generar registros
    registros = generar_registros(args.count, args.stations)
    
    # Mostrar muestra si no se deshabilitó
    if not args.no_sample:
        mostrar_muestra(registros)
    
    # Escribir archivo
    archivo_salida = Path(__file__).parent / args.output
    escribir_archivo_binario(registros, archivo_salida)
    
    print(f"\n✅ Generación completada exitosamente!")
    
    return 0


if __name__ == '__main__':
    exit(main())
