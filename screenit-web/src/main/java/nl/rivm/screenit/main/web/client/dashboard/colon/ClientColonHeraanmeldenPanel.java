
package nl.rivm.screenit.main.web.client.dashboard.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.ClientContactService;
import nl.rivm.screenit.main.service.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientNieuweAfspraakMakenPanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.radiochoice.BooleanRadioChoice;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientColonHeraanmeldenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	private Boolean nieuweUitnodiging = Boolean.FALSE;

	private ColonClientNieuweAfspraakMakenPanel afspraakMakenPanel = null;

	private IModel<ColonAfmelding> heraanmeldModel;

	@SpringBean
	private ClientContactService clientContactService;

	public ClientColonHeraanmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		ColonDossier colonDossier = client.getObject().getColonDossier();
		ColonAfmelding heraanmeld = null;

		if (DossierStatus.INACTIEF.equals(colonDossier.getStatus()))
		{
			List<ColonAfmelding> dossierAfmeldingen = colonDossier.getAfmeldingen();
			for (ColonAfmelding anderHeraanmeld : dossierAfmeldingen)
			{
				if (AanvraagBriefStatus.VERWERKT.equals(anderHeraanmeld.getAfmeldingStatus()) && anderHeraanmeld.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT)
				{
					heraanmeld = anderHeraanmeld;
					break;
				}
			}
		}
		else if (DossierStatus.ACTIEF.equals(colonDossier.getStatus()))
		{
			if (colonDossier.getLaatsteScreeningRonde() != null)
			{
				for (ColonAfmelding anderHeraanmeld : colonDossier.getLaatsteScreeningRonde().getAfmeldingen())
				{
					if (AanvraagBriefStatus.VERWERKT.equals(anderHeraanmeld.getAfmeldingStatus()) && AfmeldingType.EENMALIG.equals(anderHeraanmeld.getType())
						&& !AanvraagBriefStatus.VERWERKT.equals(anderHeraanmeld.getHeraanmeldStatus()))
					{
						heraanmeld = anderHeraanmeld;
						break;
					}
				}
			}
		}

		this.heraanmeldModel = ModelUtil.cModel(heraanmeld);

		String prefix = "definitief";
		if (heraanmeld != null && AfmeldingType.EENMALIG.equals(heraanmeld.getType()))
		{
			prefix = "eenmalig";
		}
		add(new Label("heraanmeldtekst", getString(prefix + ".heraanmeldtekst")));
		add(new Label("titelheraanmeld", getString(prefix + ".titelheraanmeld")));

		boolean magNieuweUitnodigingAanvragen = clientContactService.magNieuweUitnodigingAanvragen(colonDossier, true);
		nieuweUitnodiging = clientContactService.defaultNieuweUitnodigingAanvragen(colonDossier);

		RadioChoice<Boolean> nieuweUitnodigingRadio = new BooleanRadioChoice("nieuweUitnodiging", new PropertyModel<Boolean>(this, "nieuweUitnodiging"));
		nieuweUitnodigingRadio.setPrefix("<label class=\"radio\">");
		nieuweUitnodigingRadio.setSuffix("</label>");
		nieuweUitnodigingRadio.setOutputMarkupId(true);
		nieuweUitnodigingRadio.setVisible(magNieuweUitnodigingAanvragen);
		nieuweUitnodigingRadio.setEnabled(!nieuweUitnodiging);
		add(nieuweUitnodigingRadio);

		if (clientContactService.magNieuweIntakeAfspraakMakenNaHeraanmelding(colonDossier))
		{
			afspraakMakenPanel = new ColonClientNieuweAfspraakMakenPanel("afspraakMaken", ModelUtil.sModel(colonDossier.getLaatsteScreeningRonde().getLaatsteAfspraak()));
			add(afspraakMakenPanel);
		}
		else
		{
			add(new EmptyPanel("afspraakMaken").setVisible(false));
		}
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		if (afspraakMakenPanel != null)
		{
			opslaanObjecten.putAll(afspraakMakenPanel.getOpslaanObjecten());
		}
		opslaanObjecten.put(ExtraOpslaanKey.HERAANMELDING, heraanmeldModel.getObject());
		opslaanObjecten.put(ExtraOpslaanKey.NIEUWE_UITNODIGING, nieuweUitnodiging);
		return opslaanObjecten;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(heraanmeldModel);
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		if (afspraakMakenPanel != null)
		{
			return afspraakMakenPanel.getOpslaanMeldingen();
		}
		return new ArrayList<>();
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(OnDomReadyHeaderItem.forScript("$('#opslaanBtn').attr('disabled', true);"));
	}

}
