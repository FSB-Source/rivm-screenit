package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public enum FileType
{
	WORD_NIEUW(
		"Word-document",
		Collections.singletonList(".docx"),
		Arrays.asList("application/msword", "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "application/doc",
			"application/vnd.msword", "application/vnd.ms-word", "application/word", "application/octet-stream"),
		new byte[] { 0x50, 0x4B, 0x03, 0x04 }),

	WORD_OUD(
		"Word 97-2003-document",
		Collections.singletonList(".doc"),
		Arrays.asList("application/msword", "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "application/doc",
			"application/vnd.msword", "application/vnd.ms-word", "application/word", "application/octet-stream"),
		new byte[] { (byte) 0xD0, (byte) 0xCF, 0x11, (byte) 0xE0, (byte) 0xA1, (byte) 0xB1, 0x1A, (byte) 0xE1 }),

	PDF(
		"PDF",
		Collections.singletonList(".pdf"),
		Arrays.asList("application/pdf", "application/octet-stream"),
		new byte[] { 0x25, 0x50, 0x44, 0x46 }),

	EXCEL_NIEUW(
		"excel-werkmap",
		Collections.singletonList(".xlsx"),
		Arrays.asList("text/xls", "text/comma-separated-values", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
			"application/octet-stream"),
		new byte[] { 0x50, 0x4B, 0x03, 0x04 }),

	EXCEL_OUD(
		"Excel 97-2003-werkmap",
		Collections.singletonList(".xls"),
		Arrays.asList("text/xls", "text/comma-separated-values", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
			"application/octet-stream"),
		new byte[] { 0x50, 0x4B, 0x03, 0x04 }),

	CSV(
		"CSV",
		Arrays.asList(".csv"),
		Arrays.asList("text/csv", "text/plain", "application/csv", "application/x-mpb", "application/octet-stream", "text/anytext", "application/vnd.ms-excel"),
		null),

	JPG(
		"JPG bestand",
		Arrays.asList(".jpeg", ".jpg"),
		Arrays.asList("image/jpeg", "application/jpeg", "image/x-citrix-jpeg"),
		new byte[] { (byte) 0xFF, (byte) 0xD8, (byte) 0xFF }),

	ZIP(
		"ZIP",
		Collections.singletonList(".zip"),
		Arrays.asList("application/zip", "application/x-zip-compressed", "application/octet-stream"),
		new byte[] { 0x50, 0x4B, 0x03, 0x04 }),

	EPS(
		"EPS",
		Collections.singletonList(".eps"),
		Arrays.asList("application/postscript", "application/eps", "application/x-eps", "image/eps", "image/x-eps", "application/octet-stream"),
		new byte[] { (byte) 0xC5, (byte) 0xD0, (byte) 0xD3, (byte) 0xC6 }),

	DICOM(
		"DICOM",
		Collections.singletonList(".dcm"),
		Arrays.asList("application/dicom", "application/octet-stream"),
		new byte[] { 0x44, 0x49, 0x43, 0x4D },
		128),

	JSON(
		"JSON",
		Collections.singletonList(".json"),
		Collections.singletonList("application/json"),
		null),

	XML(
		"XML",
		Collections.singletonList(".xml"),
		Collections.singletonList("text/xml"),
		null),
		;

	private final String naamFileType;

	private final List<String> fileExtensies;

	private final List<String> allowedContentTypes;

	private final byte[] magicNumber;

	private int offset;

	FileType(String naamFileType, List<String> fileExtensies, List<String> allowedContentTypes, byte[] magicNumber)
	{
		this(naamFileType, fileExtensies, allowedContentTypes, magicNumber, 0);
	}

	FileType(String naamFileType, List<String> fileExtensies, List<String> allowedContentTypes, byte[] magicNumber, int offset)
	{
		this.naamFileType = naamFileType;
		this.fileExtensies = fileExtensies;
		this.allowedContentTypes = allowedContentTypes;
		this.magicNumber = magicNumber;
		this.offset = offset;
	}

	public String getNaamFileType()
	{
		return naamFileType;
	}

	public List<String> getFileExtensies()
	{
		return fileExtensies;
	}

	public List<String> getAllowedContentTypes()
	{
		return allowedContentTypes;
	}

	public byte[] getMagicNumber()
	{
		return magicNumber;
	}

	public int getOffset()
	{
		return offset;
	}
}
