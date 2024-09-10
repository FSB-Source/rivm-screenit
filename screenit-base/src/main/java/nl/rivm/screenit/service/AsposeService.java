package nl.rivm.screenit.service;

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

import java.io.File;

import nl.rivm.screenit.document.DocumentCreator;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;

import com.aspose.words.Document;
import com.aspose.words.PdfSaveOptions;

public interface AsposeService
{

	Document processDocument(byte[] templateDocument, MailMergeContext context) throws Exception;

	Document processDocument(File file, MailMergeContext context) throws Exception;

	boolean heeftMergeField(File templateFile, MergeField mergeField);

	Document processDocumentWithCreator(MailMergeContext context, File templateDocument, DocumentCreator creator, boolean replaceMergeFieldIfNull) throws Exception;

	Document processVragenlijst(MailMergeContext context, ScreenitFormulierInstantie vragenlijst, boolean replaceMergeFieldIfNull) throws Exception;

	PdfSaveOptions getPdfSaveOptions();

}
