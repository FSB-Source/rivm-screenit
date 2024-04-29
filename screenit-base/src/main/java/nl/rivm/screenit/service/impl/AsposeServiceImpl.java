package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;
import javax.imageio.ImageIO;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.document.DocumentCreator;
import nl.rivm.screenit.document.VragenlijstDocumentCreator;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BarcodeService;
import nl.rivm.screenit.service.UploadDocumentService;

import org.apache.commons.lang.StringUtils;
import org.krysalis.barcode4j.impl.code128.Code128Bean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.aspose.words.Document;
import com.aspose.words.FieldMergingArgs;
import com.aspose.words.IFieldMergingCallback;
import com.aspose.words.ImageFieldMergingArgs;
import com.aspose.words.License;
import com.aspose.words.PdfSaveOptions;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.WriterException;
import com.google.zxing.qrcode.QRCodeWriter;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

@Slf4j
@Service
public class AsposeServiceImpl implements AsposeService
{
	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private BarcodeService barcodeService;

	@Setter
	@Autowired
	private String asposeLicence;

	@Autowired
	private String vragenlijstTemplate;

	private PdfSaveOptions pdfSaveOptions;

	@PostConstruct
	public void init()
	{
		try
		{
			License license = new License();
			InputStream stream = new FileInputStream(asposeLicence);
			license.setLicense(stream);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	@Override
	public Document processDocument(File file, MailMergeContext context) throws Exception
	{
		Document document = null;
		try (InputStream stream = new FileInputStream(file))
		{
			document = new Document(stream);
			processDocument(document, context, true);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return document;
	}

	@Override
	public Document processDocument(byte[] templateDocument, MailMergeContext context) throws Exception
	{
		var document = new Document(new ByteArrayInputStream(templateDocument));
		processDocument(document, context, true);
		return document;
	}

	private void processDocument(Document document, MailMergeContext context, boolean replaceMergeFieldIfNull) throws Exception
	{

		try
		{
			document.getMailMerge().setFieldMergingCallback(new MailMergeImageCallback(context));
			var mergeValues = new HashMap<String, Object>();
			bepaalSamenvoegveldWaardenVoorDocument(document, context, replaceMergeFieldIfNull, mergeValues);
			var fieldNames = mergeValues.keySet().toArray(new String[0]);
			var values = mergeValues.values().toArray();
			document.getMailMerge().execute(fieldNames, values);
		}
		catch (Exception e)
		{
			LOG.error("Error bij het mergen van document, fout door: ", e);
			throw e;
		}
	}

	private void bepaalSamenvoegveldWaardenVoorDocument(Document document, MailMergeContext context, boolean replaceMergeFieldIfNull, Map<String, Object> mergeValues)
		throws Exception
	{
		String veldnaam = null;
		try
		{
			for (var fieldName : document.getMailMerge().getFieldNames())
			{
				veldnaam = fieldName;
				var afdrukObject = getAfdrukObject(fieldName, context);
				if (afdrukObject instanceof Entry)
				{
					mergeValues.put(fieldName, ((Entry<?, ?>) afdrukObject).getValue());
				}
				if (afdrukObject instanceof MergeField)
				{
					mergeValues.put(fieldName, ((MergeField) afdrukObject).getValue(context));
				}
				if (mergeValues.get(fieldName) == null && !replaceMergeFieldIfNull)
				{
					mergeValues.remove(fieldName);
				}
			}
		}
		catch (Exception e)
		{
			LOG.error("Error tijdens bepalen van waarde voor samenvoegveld: {}", veldnaam);
			throw e;
		}
	}

	@Override
	public Document processDocumentWithCreator(MailMergeContext context, File templateFile, DocumentCreator creator, boolean replaceMergeFieldIfNull)
	{
		Document document = null;
		try (InputStream stream = new FileInputStream(templateFile))
		{
			document = new Document(stream);
			creator.fillExecuteWithRegions(document);
			processDocument(document, context, replaceMergeFieldIfNull);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return document;
	}

	@Override
	public Document processVragenlijst(MailMergeContext context, ScreenitFormulierInstantie vragenlijst, boolean replaceMergeFieldIfNull) throws Exception
	{
		var creator = new VragenlijstDocumentCreator(new File(vragenlijstTemplate), vragenlijst);
		var document = creator.getDocument();
		processDocument(document, context, replaceMergeFieldIfNull);
		return document;
	}

	@Override
	public PdfSaveOptions getPdfSaveOptions()
	{
		if (pdfSaveOptions == null)
		{
			pdfSaveOptions = new PdfSaveOptions();
			pdfSaveOptions.setUseAntiAliasing(false);
			pdfSaveOptions.setUseHighQualityRendering(true);
			pdfSaveOptions.getDownsampleOptions().setResolution(300);
		}

		return pdfSaveOptions;
	}

	private Object getAfdrukObject(String fieldName, MailMergeContext context)
	{
		var mergeField = MergeField.getByFieldname(fieldName);
		if (mergeField == null && !context.getProjectAttributen().isEmpty())
		{
			for (var entry : context.getProjectAttributen().entrySet())
			{
				if (fieldName.equals(entry.getKey().getMergeField()) && entry.getValue() != null)
				{
					return entry;
				}
			}
		}
		return mergeField;
	}

	private final class MailMergeImageCallback implements IFieldMergingCallback
	{
		private final MailMergeContext context;

		private MailMergeImageCallback(MailMergeContext context)
		{
			this.context = context;
		}

		@Override
		public void fieldMerging(FieldMergingArgs field)
		{

			var mergeField = MergeField.getByFieldname(field.getFieldName());
			if (field.getFieldValue() instanceof String && mergeField != null && !mergeField.waardeTrimmen())
			{
				field.setText((String) field.getFieldValue());
			}
		}

		@Override
		public void imageFieldMerging(ImageFieldMergingArgs field)
			throws IOException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException
		{

			var afdrukObject = getAfdrukObject(field.getFieldName(), context);
			if (afdrukObject instanceof Entry)
			{
				var entry = (Entry<ProjectAttribuut, String>) afdrukObject;
				if (entry.getKey().isBarcode())
				{
					field.setImageStream(barcodeService.maakBarcodeInputStreamVoorBrief((entry).getValue(), new Code128Bean()));
				}
			}
			if (afdrukObject instanceof MergeField)
			{
				var mergeField = (MergeField) afdrukObject;
				var mergeFieldValue = mergeField.getValue(context);

				if (mergeFieldValue == null || mergeFieldValue instanceof String && StringUtils.isBlank((String) mergeFieldValue))
				{
					throw new IllegalStateException("MergeField " + mergeField + " heeft geen waarde");
				}
				if (mergeField.isBarcode())
				{
					var abstractBarcodeBean = mergeField.getBarcodeType().getConstructor().newInstance();
					field.setImageStream(barcodeService.maakBarcodeInputStreamVoorBrief(mergeFieldValue.toString(), mergeField.getBarcodeHeight(), abstractBarcodeBean));
				}
				else if (mergeField.isQrCode())
				{
					qrcodeMerger(field, mergeFieldValue.toString());
				}
				else if (mergeFieldValue instanceof UploadDocument)
				{
					imageMerger(field, (UploadDocument) mergeFieldValue);
				}
			}
		}

		private void imageMerger(ImageFieldMergingArgs field, UploadDocument uploadDocument) throws IOException
		{
			var mergeFieldFile = uploadDocumentService.load(uploadDocument);

			if (uploadDocument.getContentType().equals("image/x-eps") || uploadDocument.getContentType().equals("application/postscript"))
			{
				throw new UnsupportedOperationException("Het is niet meer mogelijk om EPS bestanden te gebruiken");
			}

			try (var inputStream = new FileInputStream(mergeFieldFile))
			{
				field.setImageStream(inputStream);
			}
			catch (Exception e)
			{
				LOG.error(e.getMessage(), e);
			}
		}

		private void qrcodeMerger(ImageFieldMergingArgs field, String message)
		{

			var outputStream = new ByteArrayOutputStream();

			if (StringUtils.isNotBlank(message))
			{
				message = Constants.LOCATIEID + "=" + message;

				int size = 100;
				try
				{
					var hintMap = new EnumMap<>(EncodeHintType.class);
					hintMap.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
					var qrCodeWriter = new QRCodeWriter();
					var byteMatrix = qrCodeWriter.encode(message, BarcodeFormat.QR_CODE, size, size, hintMap);
					int crunchifyWidth = byteMatrix.getWidth();
					var image = new BufferedImage(crunchifyWidth, crunchifyWidth, BufferedImage.TYPE_INT_RGB);
					image.createGraphics();

					var graphics = (Graphics2D) image.getGraphics();
					graphics.setColor(Color.WHITE);
					graphics.fillRect(0, 0, crunchifyWidth, crunchifyWidth);
					graphics.setColor(Color.BLACK);

					for (int x = 0; x < crunchifyWidth; x++)
					{
						for (int y = 0; y < crunchifyWidth; y++)
						{
							if (byteMatrix.get(x, y))
							{
								graphics.fillRect(x, y, 1, 1);
							}
						}
					}
					ImageIO.write(image, "png", outputStream);
				}
				catch (WriterException | IOException e)
				{
					LOG.error(e.getMessage());
				}
			}

			var inputStream = new ByteArrayInputStream(outputStream.toByteArray());
			field.setImageStream(inputStream);
		}
	}
}
