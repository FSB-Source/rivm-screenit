package nl.rivm.screenit.service.mamma.impl;

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

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseHL7v24MessageService;
import nl.rivm.screenit.service.mamma.MammaBaseIlmService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseTestService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.util.postcode.PostcodeFormatter;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class MammaBaseTestServiceImpl implements MammaBaseTestService
{
	@Autowired
	private TestService testService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Autowired
	private DossierFactory dossierFactory;

	@Autowired
	private MammaBaseDossierService mammaBaseDossierService;

	@Autowired
	private BaseDossierService baseDossierService;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaBaseIlmService baseIlmService;

	@Autowired
	private MammaBaseHL7v24MessageService baseHL7v24MessageService;

	@Override
	public MammaDossier geefDossier(GbaPersoon gbaPersoon)
	{
		var client = testService.maakClient(gbaPersoon);
		return client.getMammaDossier();
	}

	@Override
	@Transactional
	public MammaScreeningRonde geefScreeningRonde(GbaPersoon gbaPersoon)
	{
		var dossier = geefDossier(gbaPersoon);

		var ronde = dossier.getLaatsteScreeningRonde();
		if (ronde == null)
		{
			ronde = baseFactory.maakRonde(dossier, hibernateService.loadAll(MammaStandplaatsRonde.class).get(0), false);
			baseFactory.maakUitnodiging(ronde, ronde.getStandplaatsRonde(), BriefType.MAMMA_OPEN_UITNODIGING);
			verzendLaatsteBrief(ronde);
		}
		return ronde;
	}

	@Override
	@Transactional
	public MammaUitnodiging maakUitnodiging(GbaPersoon gbaPersoon, BriefType briefType)
	{
		var ronde = geefScreeningRonde(gbaPersoon);

		var uitnodiging = baseFactory.maakUitnodiging(ronde, ronde.getStandplaatsRonde(), briefType);
		verzendLaatsteBrief(ronde);
		return uitnodiging;
	}

	private void verzendLaatsteBrief(MammaScreeningRonde ronde)
	{
		var brief = ronde.getLaatsteBrief();

		var mergedBrieven = new MammaMergedBrieven();
		mergedBrieven.setCreatieDatum(dateSupplier.getDate());
		mergedBrieven.setBriefType(BriefType.MAMMA_OPEN_UITNODIGING);
		mergedBrieven.setPrintDatum(dateSupplier.getDate());
		mergedBrieven.setVerwijderd(true);
		brief.setMergedBrieven(mergedBrieven);
		var fakeMergeDocument = new UploadDocument();
		fakeMergeDocument.setActief(true);
		fakeMergeDocument.setNaam("dummy_testservice_brief_niet_openen");
		hibernateService.saveOrUpdate(fakeMergeDocument);
		mergedBrieven.setMergedBrieven(fakeMergeDocument);
		hibernateService.saveOrUpdate(mergedBrieven);
		hibernateService.saveOrUpdate(brief);
	}

	@Override
	@Transactional
	public void clientReset(Client client, boolean verwijderAlleBerichten)
	{
		var dossier = client.getMammaDossier();
		if (dossier == null)
		{
			return;
		}

		mammaBaseDossierService.maakDossierLeeg(dossier);

		baseDossierService.verwijderLaatsteAfmelding(dossier);

		baseIlmService.verwijderIlmBezwaarPogingen(dossier);

		baseIlmService.verwijderIlmRapportageEntriesVoorClient(client);

		var deelnamekans = dossier.getDeelnamekans();
		if (deelnamekans != null)
		{
			hibernateService.delete(deelnamekans);
		}

		baseHL7v24MessageService.verwijderBerichtVoorClient(client, verwijderAlleBerichten);

		var overgeblevenBrieven = hibernateService.getByParameters(MammaBrief.class, Map.of("client", client));
		hibernateService.deleteAll(overgeblevenBrieven);

		client.setMammaDossier(null);
		hibernateService.delete(dossier);
		hibernateService.saveOrUpdate(client);

		dossierFactory.maakDossiers(client);

		LOG.info("Client gereset met id: " + client.getId());
	}

	@Override
	@Transactional
	public int clientenDefinitiefAfmelden(List<Client> clienten, MammaAfmeldingReden afmeldingReden)
	{
		var aantalAfgemeld = 0;
		for (var client : clienten)
		{
			var afmelding = new MammaAfmelding();
			afmelding.setDossier(client.getMammaDossier());
			afmelding.setType(AfmeldingType.DEFINITIEF);
			afmelding.setManier(ClientContactManier.DIRECT);
			afmelding.setReden(afmeldingReden);
			baseAfmeldService.afmeldenZonderVervolg(client, afmelding, false, null);

			aantalAfgemeld++;
		}
		return aantalAfgemeld;
	}

	@Override
	@Transactional
	public void maakOfVindClient(String bsn, MammaDoelgroep doelgroep, String postcode, BigDecimal deelnamekans, BigDecimal opkomstkans, Date geboortedatum)
	{
		var persoon = new GbaPersoon();
		persoon.setBsn(bsn);
		persoon.setGeslacht(Geslacht.VROUW);
		persoon.setGeboortedatum(geboortedatum);

		var adres = new BagAdres();
		adres.setPostcode(postcode);
		persoon.setGbaAdres(adres);

		var client = maakOfVindClient(persoon, doelgroep, deelnamekans, opkomstkans, false);

		postcode = PostcodeFormatter.formatPostcode(postcode, false);
		if (StringUtils.isNotBlank(postcode))
		{

			var gbaAdres = client.getPersoon().getGbaAdres();
			gbaAdres.setPostcode(postcode);
			hibernateService.saveOrUpdate(gbaAdres);
		}
	}

	@Override
	@Transactional
	public Client maakOfVindClient(GbaPersoon persoon, MammaDoelgroep doelgroep, BigDecimal deelnamekans, BigDecimal opkomstkans, boolean alleenMaken)
	{
		var client = testService.getClientByBsn(persoon.getBsn());
		var bestaandeClient = client != null;

		if (client == null || !alleenMaken)
		{
			client = testService.maakClient(persoon);
		}

		if (client.getMammaDossier() == null)
		{
			dossierFactory.maakBmhkEnBkDossiers(client);
			baseKansberekeningService.maakDossierEvent(client.getMammaDossier());
		}

		if (deelnamekans != null && doelgroep != null && alleenMaken)
		{
			var dossier = client.getMammaDossier();

			if (!bestaandeClient)
			{
				dossier.setDoelgroep(doelgroep);
				var dossierDeelnamekans = dossier.getDeelnamekans();
				dossierDeelnamekans.setDeelnamekans(deelnamekans);

				if (doelgroep.equals(MammaDoelgroep.DUBBELE_TIJD))
				{
					dossier.setDubbeleTijdReden("Test cliënt");
				}
				hibernateService.saveOrUpdateAll(dossier, dossierDeelnamekans);
			}

			for (var screeningRonde : dossier.getScreeningRondes())
			{
				for (var uitnodiging : screeningRonde.getUitnodigingen())
				{
					for (var afspraak : uitnodiging.getAfspraken())
					{
						var afspraakOpkomstkans = afspraak.getOpkomstkans();
						afspraakOpkomstkans.setOpkomstkans(opkomstkans);
						hibernateService.saveOrUpdate(afspraakOpkomstkans);
					}
				}
			}
		}

		return client;
	}
}
