package nl.rivm.screenit.mamma.se.proxy.dicom.store;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.IOException;
import java.util.Collections;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.model.DenseWaarde;
import nl.rivm.screenit.mamma.se.proxy.model.DensiteitMetingAction;
import nl.rivm.screenit.mamma.se.proxy.model.SETransactieType;
import nl.rivm.screenit.mamma.se.proxy.model.TransactieDto;
import nl.rivm.screenit.mamma.se.proxy.services.TransactionQueueService;
import nl.rivm.screenit.mamma.se.proxy.util.TransactionSerializer;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.UID;
import org.dcm4che3.net.Association;
import org.dcm4che3.net.PDVInputStream;
import org.dcm4che3.net.Status;
import org.dcm4che3.net.pdu.PresentationContext;
import org.dcm4che3.net.service.BasicCStoreSCP;
import org.dcm4che3.net.service.DicomServiceException;

@Slf4j
public class DenseStoreProvider extends BasicCStoreSCP
{
	private final TransactionQueueService transactionQueueService;

	public DenseStoreProvider(TransactionQueueService transactionQueueService)
	{
		super(UID.MammographyCADSRStorage);
		this.transactionQueueService = transactionQueueService;
	}

	@Override
	protected void store(Association as, PresentationContext pc, Attributes rq, PDVInputStream data, Attributes rsp) throws IOException
	{
		try
		{
			var structuredReport = data.readDataset(pc.getTransferSyntax());
			var denseReportParser = new DenseReportParser(structuredReport);

			var accessionNumber = denseReportParser.getAccessionNumber();
			var denseValue = denseReportParser.getDenseValueBothBreasts();
			if (accessionNumber.isPresent() && denseValue.isPresent())
			{
				queueDensewaardeTransactie(accessionNumber.get(), denseValue.get());
			}
		}
		catch (Exception e)
		{
			throw new DicomServiceException(Status.ProcessingFailure, e);
		}
	}

	private void queueDensewaardeTransactie(Long accessionNumber, DenseWaarde densiteit)
	{
		var transactieDto = new TransactieDto(SETransactieType.STRUCTURED_REPORT_ONTVANGEN, accessionNumber.toString(),
			Collections.singletonList(new DensiteitMetingAction(accessionNumber, densiteit)));

		var json = TransactionSerializer.writeAsString(transactieDto);
		transactionQueueService.addTransactionToQueue(json, null);
	}
}
