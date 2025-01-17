package nl.rivm.screenit.batch.service.impl.dicom;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.IOException;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.UID;
import org.dcm4che3.data.VR;
import org.dcm4che3.io.DicomEncodingOptions;
import org.dcm4che3.io.DicomInputStream;
import org.dcm4che3.io.DicomInputStream.IncludeBulkData;
import org.dcm4che3.media.DicomDirReader;
import org.dcm4che3.media.DicomDirWriter;
import org.dcm4che3.media.RecordFactory;
import org.dcm4che3.media.RecordType;
import org.dcm4che3.util.SafeClose;
import org.dcm4che3.util.UIDUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DicomDir
{
	private static final Logger LOG = LoggerFactory.getLogger(DicomDir.class);

	private File file;

	private DicomDirReader in;

	private DicomDirWriter out;

	private RecordFactory recFact = new RecordFactory();

	private final String dicomDirRootPath;

	public DicomDir(String dicomDirRootPath)
	{
		this.dicomDirRootPath = dicomDirRootPath;
	}

	public void close()
	{
		SafeClose.close(in);
		in = null;
		out = null;
	}

	public void create() throws IOException
	{
		this.file = new File(dicomDirRootPath + File.separator + "DICOMDIR");
		DicomDirWriter.createEmptyDirectory(file,
			UIDUtils.createUIDIfNull(null),
			null,
			null,
			null);
		in = out = DicomDirWriter.open(file);
		out.setEncodingOptions(DicomEncodingOptions.DEFAULT);
		addReferenceTo(new File(dicomDirRootPath));
	}

	private int addReferenceTo(File f) throws IOException
	{
		checkOut();
		checkRecordFactory();
		int n = 0;
		if (f.isDirectory())
		{
			for (String s : f.list())
			{
				n += addReferenceTo(new File(f, s));
			}
			return n;
		}

		if (f.equals(file))
		{
			return 0;
		}

		Attributes fmi;
		Attributes dataset;
		DicomInputStream din = null;
		try
		{
			din = new DicomInputStream(f);
			din.setIncludeBulkData(IncludeBulkData.NO);
			fmi = din.readFileMetaInformation();
			dataset = din.readDatasetUntilPixelData();
		}
		catch (IOException e)
		{
			LOG.error("Fout bij laden beeld", e);
			return 0;
		}
		finally
		{
			if (din != null)
			{
				try
				{
					din.close();
				}
				catch (Exception ignore)
				{
				}
			}
		}
		char prompt = '.';
		if (fmi == null)
		{
			fmi = dataset.createFileMetaInformation(UID.ImplicitVRLittleEndian);
			prompt = 'F';
		}
		String iuid = fmi.getString(Tag.MediaStorageSOPInstanceUID, null);
		if (iuid == null)
		{
			LOG.warn("File overgeslagen " + f.getPath());
			return 0;
		}

		return addRecords(dataset, n, out.toFileIDs(f), prompt, iuid, fmi);
	}

	private int addRecords(Attributes dataset, int num, String[] fileIDs, char prompt, String iuid, Attributes fmi) throws IOException
	{
		String pid = dataset.getString(Tag.PatientID, null);
		String styuid = dataset.getString(Tag.StudyInstanceUID, null);
		String seruid = dataset.getString(Tag.SeriesInstanceUID, null);

		if (styuid != null)
		{
			if (pid == null)
			{
				dataset.setString(Tag.PatientID, VR.LO, pid = styuid);
				prompt = prompt == 'F' ? 'P' : 'p';
			}
			Attributes patRec = in.findPatientRecord(pid);
			if (patRec == null)
			{
				patRec = recFact.createRecord(RecordType.PATIENT, null, dataset, null, null);
				out.addRootDirectoryRecord(patRec);
				num++;
			}
			Attributes studyRec = in.findStudyRecord(patRec, styuid);
			if (studyRec == null)
			{
				studyRec = recFact.createRecord(RecordType.STUDY, null, dataset, null, null);
				out.addLowerDirectoryRecord(patRec, studyRec);
				num++;
			}

			if (seruid != null)
			{
				Attributes seriesRec = in.findSeriesRecord(studyRec, seruid);
				if (seriesRec == null)
				{
					seriesRec = recFact.createRecord(RecordType.SERIES, null, dataset, null, null);
					out.addLowerDirectoryRecord(studyRec, seriesRec);
					num++;
				}

				if (iuid != null)
				{
					Attributes instRec;
					instRec = recFact.createRecord(dataset, fmi, fileIDs);
					out.addLowerDirectoryRecord(seriesRec, instRec);
					num++;
				}
			}
		}
		else
		{
			if (iuid != null)
			{
				Attributes instRec = recFact.createRecord(dataset, fmi, fileIDs);
				out.addRootDirectoryRecord(instRec);
				prompt = prompt == 'F' ? 'R' : 'r';
				num++;
			}
		}
		System.out.print(prompt);
		return num;
	}

	private void checkIn()
	{
		if (in == null)
		{
			throw new IllegalStateException("Geen open file");
		}
	}

	private void checkOut()
	{
		checkIn();
		if (out == null)
		{
			throw new IllegalStateException("File is read only");
		}
	}

	private void checkRecordFactory()
	{
		if (recFact == null)
		{
			throw new IllegalStateException("Geen recordFactory");
		}
	}

}
