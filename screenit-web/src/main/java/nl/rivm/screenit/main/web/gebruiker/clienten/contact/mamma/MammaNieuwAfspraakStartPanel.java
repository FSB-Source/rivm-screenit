package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class MammaNieuwAfspraakStartPanel extends GenericPanel<MammaUitstel>
{

	private IModel<Client> clientModel;

	public MammaNieuwAfspraakStartPanel(String id, IModel<Client> model)
	{
		super(id);
		this.clientModel = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		MammaDossier dossier = clientModel.getObject().getMammaDossier();
		MammaScreeningRonde laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
		MammaAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
		MammaUitstel laatsteUitstel = laatsteScreeningRonde.getLaatsteUitstel();

		boolean heeftUitstel = (laatsteAfspraak == null || MammaAfspraakStatus.UITGESTELD == laatsteAfspraak.getStatus()) && laatsteUitstel != null
			&& laatsteUitstel.getGeannuleerdOp() == null && laatsteUitstel.getUitnodiging() == null;

		add(new Label("rondeForcerenMelding", getString("uitlegRondeForceren")).setVisible(rondeForcerenMeldingTonen()));

		setModel(ModelUtil.csModel(laatsteUitstel));
		add(DateLabel.forDatePattern("streefDatum", "EEEE dd-MM-yyyy").setVisible(heeftUitstel));

		if (heeftUitstel)
		{
			MammaStandplaats standplaats = laatsteUitstel.getStandplaats();
			add(new StandplaatsFragment("standplaats", laatsteUitstel.getStreefDatum(), standplaats));
		}
		else
		{
			add(new EmptyPanel("standplaats"));
		}

		IndicatingAjaxLink<Void> aanmakenLink = new IndicatingAjaxLink<>("aanmaken")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				afspraakAanmaken(target, clientModel);
			}
		};

		add(aanmakenLink);

		IndicatingAjaxLink<Void> uitstellenLink = new IndicatingAjaxLink<>("uitstellen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				uitstellen(target, clientModel);
			}
		};

		uitstellenLink.setVisible(magUitstellen());
		add(uitstellenLink);
	}

	public abstract void afspraakAanmaken(AjaxRequestTarget target, IModel<Client> clientModel);

	public abstract void uitstellen(AjaxRequestTarget target, IModel<Client> clientModel);

	protected abstract boolean magUitstellen();

	protected boolean rondeForcerenMeldingTonen()
	{
		return false;
	}

	private class StandplaatsFragment extends Fragment
	{

		public StandplaatsFragment(String id, Date vanaf, MammaStandplaats standplaats)
		{
			super(id, "standplaatsFragment", MammaNieuwAfspraakStartPanel.this);

			MammaStandplaatsLocatie locatie = null;

			add(new Label("standplaats.naam", standplaats.getNaam()));
			MammaStandplaatsLocatie tijdelijkeLocatie = standplaats.getTijdelijkeLocatie();
			if (tijdelijkeLocatie.getStartDatum() != null)
			{
				Date eindDatum = tijdelijkeLocatie.getEindDatum();
				eindDatum.setHours(23);
				eindDatum.setMinutes(59);
				if (tijdelijkeLocatie.getStartDatum().compareTo(vanaf) * vanaf.compareTo(eindDatum) > 0)
				{
					locatie = tijdelijkeLocatie;
				}
			}
			if (locatie == null)
			{
				locatie = standplaats.getLocatie();
			}

			IModel<MammaStandplaatsLocatie> locatieModel = ModelUtil.csModel(locatie);

			WebMarkupContainer locatieContainer = new WebMarkupContainer("locatieContainer", locatieModel);
			add(locatieContainer);
			locatieContainer.add(new Label("straat"));
			locatieContainer.add(new Label("huisnummer"));
			locatieContainer.add(new Label("postcode"));
			locatieContainer.add(new Label("plaats"));
			locatieContainer.add(new Label("locatieBeschrijving"));

			WebMarkupContainer tijdelijkContainer = new WebMarkupContainer("tijdelijkContainer");
			locatieContainer.add(tijdelijkContainer);
			tijdelijkContainer.setVisible(locatie.getTijdelijk());
			tijdelijkContainer.add(DateLabel.forDatePattern("startDatum", "dd-MM-yyyy"));
			tijdelijkContainer.add(DateLabel.forDatePattern("eindDatum", "dd-MM-yyyy"));
		}
	}

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(clientModel);
	}
}
