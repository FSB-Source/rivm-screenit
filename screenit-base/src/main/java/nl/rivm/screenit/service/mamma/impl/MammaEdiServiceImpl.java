package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.edi.model.MedVryOut;
import nl.rivm.screenit.edi.model.OutboundMessageData;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.MedVryOntvanger;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht;
import nl.rivm.screenit.model.mamma.enums.MammaHuisartsBerichtStatus;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.impl.EdiServiceBaseImpl;
import nl.rivm.screenit.service.mamma.MammaEdiService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaEdiServiceImpl extends EdiServiceBaseImpl implements MammaEdiService
{

	private static final Logger LOG = LoggerFactory.getLogger(MammaEdiServiceImpl.class);

	private static final ExecutorService executorService = Executors.newSingleThreadExecutor();

	@Autowired
	private ClientService clientService;

	@Override
	public void verstuurMedVry(MammaHuisartsBericht huisartsBericht, boolean async)
	{
		if (huisartsBericht != null)
		{
			String transactionId = Long.toString(currentDateSupplier.getDate().getTime());

			nl.rivm.screenit.edi.model.MedVryOut medVry = maakMedVry(huisartsBericht);
			zetPatient(huisartsBericht, medVry);
			zetInhoud(huisartsBericht.getBerichtInhoud(), huisartsBericht.getBerichtType(), medVry, transactionId);
			if (async)
			{
				LOG.info("Verstuur huisartsbericht async");
				verstuurAsync(huisartsBericht.getId(), transactionId, medVry);
			}
			else
			{
				LOG.info("Verstuur huisartsbericht sync");
				verstuurEdiBericht(huisartsBericht, transactionId, medVry);
			}
		}
	}

	private void verstuurAsync(Long huisartsBerichtId, String transactionId, MedVryOut medVry)
	{
		LOG.debug("Start nieuwe BK HA bericht versturen thread voor bericht id {}, transaction id {} 1/3", huisartsBerichtId, transactionId);
		executorService.submit(new OpenHibernate5SessionInThread()
		{
			@Override
			protected void runInternal()
			{
				try
				{
					LOG.debug("Thread start BK HA voor bericht id {}, transaction id {} 2/3", huisartsBerichtId, transactionId);
					MammaHuisartsBericht huisartsBericht = null;
					for (int i = 0; i < 8 && huisartsBericht == null; i++)
					{
						huisartsBericht = hibernateService.get(MammaHuisartsBericht.class, huisartsBerichtId);
						Thread.sleep(250);
					}
					if (huisartsBericht == null)
					{
						LOG.warn("Geen huisartsbericht gevonden, huisartsbericht {} zal worden opgepakt door de batch", huisartsBerichtId);
						return;
					}

					if (huisartsBericht.getStatusDatum().compareTo(currentDateSupplier.getDateTime().minusMinutes(Constants.BK_HA_BERICHT_MAX_WAITTME).toDate()) < 0
						&& MammaHuisartsBerichtStatus.AANGEMAAKT.equals(huisartsBericht.getStatus()))
					{
						LOG.debug("Thread BK HA bericht. Bericht wordt niet verstuurd via async.");
						return;
					}
					verstuurEdiBericht(huisartsBericht, transactionId, medVry);
					LOG.debug("Thread klaar BK HA voor bericht id {}, transaction id {} 3/3", huisartsBerichtId, transactionId);
				}
				catch (Exception e)
				{
					LOG.error("Fout bij het versturen van BK Ha bericht {}", huisartsBerichtId, e);
				}
			}
		});
	}

	private void verstuurEdiBericht(MammaHuisartsBericht huisartsBericht, String transactionId, MedVryOut medVry)
	{
		LOG.debug("[BK HA] verzamel data edi bericht");
		InstellingGebruiker sender = zetZender(huisartsBericht, medVry);
		zetOntvanger(huisartsBericht, medVry);
		OutboundMessageData<MedVryOut> outboundMessageData = new OutboundMessageData<>(medVry);
		outboundMessageData.setSubject(medVry.getSubject());
		outboundMessageData.setAddress(medVry.getMail());
		LOG.debug("[BK HA] Verzend check");
		String foutmelding = verzendCheck(medVry, huisartsBericht.getScreeningsOrganisatie());
		try
		{
			LOG.debug("[BK HA] Versturen medvry bericht");
			verzendenEdiMedVryBericht(outboundMessageData, foutmelding, medVry, sender, transactionId, huisartsBericht);
		}
		catch (Exception e) 
		{
			huisartsBericht.setStatus(MammaHuisartsBerichtStatus.VERSTUREN_MISLUKT);
			LOG.error("Het is niet gelukt een EDI bericht verzonden voor HuisartsBericht met ID: " + huisartsBericht.getId(), e);
		}
		finally
		{
			huisartsBericht.setStatusDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().plusSeconds(1)));
			hibernateService.saveOrUpdate(huisartsBericht);

			logHuisartsberichtStatus(foutmelding, huisartsBericht, medVry);
		}
		LOG.debug("[BK HA] Klaar met versturen bericht");
	}

	private void verzendenEdiMedVryBericht(OutboundMessageData<MedVryOut> outboundMessageData, String foutmelding, MedVryOut medVry, InstellingGebruiker sender,
		String transactionId, MammaHuisartsBericht huisartsBericht)
	{
		MailVerzenden mailVerzenden = manipulateEmailadressen(sender, outboundMessageData);
		if (StringUtils.isBlank(foutmelding)
			&& (MailVerzenden.UIT.equals(mailVerzenden) || ediMessageService.sendMedVry(sender, sender.getEmail(), outboundMessageData, transactionId)))
		{
			huisartsBericht.setBerichtInhoud("");
			huisartsBericht.setStatus(MammaHuisartsBerichtStatus.VERSTUURD);
			huisartsBericht.setVerzendDatum(currentDateSupplier.getDate());
			LOG.info("Er is succesvol een EDI bericht verzonden voor HuisartsBericht met ID: " + huisartsBericht.getId());
		}
		else
		{
			huisartsBericht.setStatus(MammaHuisartsBerichtStatus.VERSTUREN_MISLUKT);
			LOG.error("Het is niet gelukt een EDI bericht te verzenden voor HuisartsBericht met ID: " + huisartsBericht.getId() + ". " + foutmelding);
		}
	}

	private void logHuisartsberichtStatus(String foutmelding, MammaHuisartsBericht huisartsBericht, MedVryOut medVry)
	{
		final List<Instelling> instellingList = addRivmInstelling(new ArrayList<>());
		instellingList.addAll(clientService.getScreeningOrganisatieVan(huisartsBericht.getClient()));

		final String enovationEdiAdres = huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres();
		if (MammaHuisartsBerichtStatus.VERSTUURD.equals(huisartsBericht.getStatus()))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HUISARTSBERICHTEN_VERZONDEN,
				instellingList,
				huisartsBericht.getClient(),
				getLoggingTekst(huisartsBericht, foutmelding, enovationEdiAdres, medVry.getReceiverId()),
				Bevolkingsonderzoek.MAMMA);
		}
		else if (MammaHuisartsBerichtStatus.VERSTUREN_MISLUKT.equals(huisartsBericht.getStatus()))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HUISARTSBERICHTEN_NIET_VERZONDEN,
				instellingList,
				huisartsBericht.getClient(),
				getLoggingTekst(huisartsBericht, foutmelding, enovationEdiAdres, medVry.getReceiverId()),
				Bevolkingsonderzoek.MAMMA);
		}
	}

	private void zetOntvanger(MammaHuisartsBericht huisartsBericht, MedVryOut medVryOut)
	{
		MedVryOntvanger ontvanger = null;
		if (huisartsBericht.getHuisarts() != null)
		{
			ontvanger = new MedVryOntvanger(huisartsBericht.getHuisarts());
			medVryOut.setReceiverId(huisartsBericht.getHuisarts().getKlantnummer());
		}
		if (ontvanger != null)
		{
			medVryOut.setOntvanger(ontvanger);
			medVryOut.setMail(ontvanger.getEdiMailAdres());
		}
	}

	@Override
	public MammaHuisartsBericht maakHuisartsBericht(Client client, MailMergeContext context, MammaHuisartsBericht huisartsBericht)
	{
		String berichtInhoud = merge(context, huisartsBericht.getBerichtType());
		huisartsBericht.setBerichtInhoud(berichtInhoud);

		client.getHuisartsBerichten().add(huisartsBericht);
		hibernateService.saveOrUpdateAll(client);
		return huisartsBericht;
	}

	private String getLoggingTekst(MammaHuisartsBericht haBericht, String foutmelding, String afzender, String ontvanger)
	{
		StringBuilder logtekst = new StringBuilder();
		if (haBericht.getHuisarts() != null)
		{
			logtekst.append("Huisarts: ");
			logtekst.append(NaamUtil.getNaamHuisarts(haBericht.getHuisarts()));
			logtekst.append(", ");
		}
		return getAlgemeneLoggingTekst(haBericht.getBerichtType(), foutmelding, afzender, ontvanger, logtekst);
	}
}
