import socket
import struct
import optparse

def prepare_sock(port):
	udp_sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
	udp_sock.bind(('0.0.0.0', 0))
	udp_sock.connect(('0.0.0.0', port))
	return udp_sock

if __name__ == '__main__':
	parser = optparse.OptionParser()
	parser.add_option('-p', '--port', dest='port', default=9900, type='int', help='The port to send to')
	opts, args = parser.parse_args()
	udp_sock = prepare_sock(opts.port)
	udp_sock.send('NEW')
	buf = udp_sock.recv(100)
	print buf
