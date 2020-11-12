package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import com.aspose.words.Document;

public interface IBrievenGeneratorHelper<B extends Brief, MB extends MergedBrieven<?>>
{

	BaseDocumentCreator getDocumentCreator(MailMergeContext context);

	void additionalActiesWithDocument(MailMergeContext context, B brief, Document chunkDocument) throws Exception;

	String getTechnischeLoggingMergedBriefAanmaken(MB mergedBrieven);

	String getMergedBrievenNaam(MB mergedBrieven);

	Long getFileStoreId();

	void crashMelding(String melding, Exception e);

	void verhoogAantalBrievenVanScreeningOrganisatie(MB mergedBrieven);

	void additionalMergedContext(MailMergeContext context);

	Bevolkingsonderzoek[] getBevolkingsonderzoeken();

	LogGebeurtenis getMergeProbleemLogGebeurtenis();

	LogGebeurtenis getOnvolledigAdresLogGebeurtenis();

	FileStoreLocation getFileStoreLocation();

	IDocument getDocumentDefinitie();

	MB getMergedBrieven();

	MB createMergedBrieven(Date aangemaaktOp);

	void increasePdfCounter();

}
