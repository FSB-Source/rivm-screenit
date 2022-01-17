package nl.rivm.screenit.main.web.gebruiker.clienten.contact;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Map;

import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientNieuweAfspraakMakenPanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.service.ClientContactService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.radiochoice.BooleanRadioChoice;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class AbstractClientContactHeraanmeldenPanel<D extends Dossier<? extends S, ? extends A>, A extends Afmelding<? extends S, ? extends D, ? extends ClientBrief<? extends S, ? extends A, ? extends C>>, C extends ClientBrief<? extends S, ? extends A, ? extends C>, S extends ScreeningRonde<? extends D, ? extends ClientBrief<? extends S, ? extends A, ? extends C>, ? extends A, ? extends Uitnodiging<? extends S>>>
	extends AbstractClientContactActiePanel<ClientContactActie>
{
	private static final long serialVersionUID = 1L;

	private final IModel<Client> clientModel;

	@SpringBean
	private ClientContactService clientContactService;

	private Boolean nieuweUitnodiging;

	protected ColonClientNieuweAfspraakMakenPanel afspraakMakenPanel = null;

	protected IModel<A> herAanTeMeldenAfmeldingModel;

	public AbstractClientContactHeraanmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> clientModel)
	{
		super(id, model);
		this.clientModel = clientModel;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		D dossier = getDossier(clientModel.getObject());

		A herAanTeMeldenAfmelding = null;

		for (A afmelding : dossier.getAfmeldingen())
		{

			if (AanvraagBriefStatus.VERWERKT.equals(afmelding.getAfmeldingStatus()) && afmelding.getHeraanmeldStatus() == null)
			{
				herAanTeMeldenAfmelding = afmelding;
				break;
			}
		}
		if (herAanTeMeldenAfmelding == null)
		{
			for (A afmelding : dossier.getLaatsteScreeningRonde().getAfmeldingen())
			{

				if (AanvraagBriefStatus.VERWERKT.equals(afmelding.getAfmeldingStatus()) && afmelding.getHeraanmeldStatus() == null)
				{
					herAanTeMeldenAfmelding = afmelding;
					break;
				}
			}
		}
		this.herAanTeMeldenAfmeldingModel = ModelUtil.ccModel(herAanTeMeldenAfmelding);

		add(new Label("heraanmeldtekst", "heraanmelden"));

		nieuweUitnodiging = clientContactService.defaultNieuweUitnodigingAanvragen(dossier);

		RadioChoice<Boolean> nieuweUitnodigingRadio = new BooleanRadioChoice("nieuweUitnodiging", new PropertyModel<>(this, "nieuweUitnodiging"));
		nieuweUitnodigingRadio.setPrefix("<label class=\"radio\">");
		nieuweUitnodigingRadio.setSuffix("</label>");
		nieuweUitnodigingRadio.setOutputMarkupId(true);
		nieuweUitnodigingRadio.setVisible(magColonNieuweUitnodigingAanvragen(dossier));
		nieuweUitnodigingRadio.setEnabled(!nieuweUitnodiging);
		add(nieuweUitnodigingRadio);

		boolean afspraakMaken = false;

		if (dossier.getBevolkingsonderzoek() == Bevolkingsonderzoek.COLON)
		{
			ColonDossier colonDossier = (ColonDossier) dossier;
			if (clientContactService.magNieuweIntakeAfspraakMakenNaHeraanmelding(colonDossier))
			{
				afspraakMaken = true;
				afspraakMakenPanel = new ColonClientNieuweAfspraakMakenPanel("afspraakMaken", ModelUtil.sModel(colonDossier.getLaatsteScreeningRonde().getLaatsteAfspraak()));
				add(afspraakMakenPanel);
			}
		}

		if (!afspraakMaken)
		{
			add(new EmptyPanel("afspraakMaken").setVisible(false));
		}

	}

	private boolean magColonNieuweUitnodigingAanvragen(D dossier)
	{
		return dossier.getBevolkingsonderzoek() == Bevolkingsonderzoek.COLON && clientContactService.magNieuweUitnodigingAanvragen(dossier, true);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		if (afspraakMakenPanel != null)
		{
			opslaanObjecten.putAll(afspraakMakenPanel.getOpslaanObjecten());
		}
		A herAanTeMeldenAfmelding = herAanTeMeldenAfmeldingModel.getObject();
		herAanTeMeldenAfmelding.setClientWilNieuweUitnodiging(nieuweUitnodiging);

		opslaanObjecten.put(ExtraOpslaanKey.HERAANMELDING, herAanTeMeldenAfmelding);
		opslaanObjecten.put(ExtraOpslaanKey.NIEUWE_UITNODIGING, nieuweUitnodiging);
		return opslaanObjecten;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(herAanTeMeldenAfmeldingModel);
		ModelUtil.nullSafeDetach(clientModel);
	}

	protected abstract D getDossier(Client client);
}
