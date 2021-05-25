package nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixClientContactHerdrukPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	private IModel<CervixBrief> cervixBrief;

	@SpringBean
	private RondeNummerService rondeNummerService;

	@SpringBean
	private ClientService clientService;

	public CervixClientContactHerdrukPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		Date datum = null;
		CervixDossier cervixDossier = client.getObject().getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();
		CervixUitnodiging laatsteAfgedrukteUitstrijkjeUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(laatsteScreeningRonde, false);
		CervixBrief brief = laatsteAfgedrukteUitstrijkjeUitnodiging.getBrief();
		CervixMergedBrieven mergedBrieven = brief.getMergedBrieven();
		if (mergedBrieven.getPrintDatum() != null)
		{
			datum = mergedBrieven.getPrintDatum();
		}
		else
		{
			datum = mergedBrieven.getCreatieDatum();
		}
		CervixMonster uitstrijkje = laatsteAfgedrukteUitstrijkjeUitnodiging.getMonster();
		cervixBrief = ModelUtil.sModel(laatsteAfgedrukteUitstrijkjeUitnodiging.getBrief());
		String extraOmschrijving = "Uitnodiging-id: " + laatsteAfgedrukteUitstrijkjeUitnodiging.getUitnodigingsId() + ", Monster-id: " + uitstrijkje.getMonsterId();
		if (cervixBrief.getObject().getTemplateNaam() != null)
		{
			extraOmschrijving += ", " + cervixBrief.getObject().getTemplateNaam();
		}

		int rondeNr = rondeNummerService.geefRondeNummer(laatsteScreeningRonde);

		add(DateLabel.forDatePattern("datum", Model.of(datum), "dd-MM-yyyy HH:mm:ss"));
		add(new EnumLabel<TypeGebeurtenis>("gebeurtenis",
			cervixBrief.getObject().getMergedBrieven().getPrintDatum() != null ? TypeGebeurtenis.BRIEF_AFGEDRUKT : TypeGebeurtenis.BRIEF_KLAARGEZET));
		add(new Label("extraOmschrijving", extraOmschrijving).setVisible(extraOmschrijving != null));
		add(new WebMarkupContainer("gbaMessageContainer").setVisible(!GbaStatus.INDICATIE_AANWEZIG.equals(client.getObject().getGbaStatus())));
		add(new Label("rondeNr", rondeNr));
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		opslaanObjecten.put(ExtraOpslaanKey.CERVIX_HERDRUK_BRIEF, cervixBrief.getObject());
		return opslaanObjecten;
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> opslaanMeldingen = super.getOpslaanMeldingen();
		opslaanMeldingen.add(getString("opslaan.melding"));
		return opslaanMeldingen;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(cervixBrief);
	}
}
