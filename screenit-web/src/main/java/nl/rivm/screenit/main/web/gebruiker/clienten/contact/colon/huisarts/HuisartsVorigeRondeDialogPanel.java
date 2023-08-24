package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonHuisartsWijzigenPanel;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.annot.objects.Transient;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Setter
@Getter
public abstract class HuisartsVorigeRondeDialogPanel extends GenericPanel<ColonScreeningRonde>
{
	private IModel<EnovationHuisarts> geselecteerdModel;

	@Transient
	private IModel<EnovationHuisarts> zoekModel;

	@SpringBean
	private HibernateService hibernateService;

	private BootstrapDialog dialog;

	private ColonHuisartsWijzigenPanel huisartsWijzigenPanel;

	public HuisartsVorigeRondeDialogPanel(String id, IModel<ColonScreeningRonde> colonScreeningRonde, IModel<EnovationHuisarts> geselecteerdModel,
		IModel<EnovationHuisarts> zoekModel,
		BootstrapDialog dialog,
		ColonHuisartsWijzigenPanel huisartsWijzigenPanel)
	{
		super(id, colonScreeningRonde);
		setDialog(dialog);
		setGeselecteerdModel(geselecteerdModel);
		setZoekModel(zoekModel);
		setHuisartsWijzigenPanel(huisartsWijzigenPanel);

		EnovationHuisarts ha = getGeselecteerdModel().getObject();
		add(new Label("huisartsNaam", NaamUtil.getNaamHuisarts(ha)));
		add(new Label("praktijkNaam", new PropertyModel<String>(getGeselecteerdModel(), "praktijknaam")));
		add(new Label("praktijkAdres", AdresUtil.getVolledigeAdresString(ha.getAdres())));
		add(new Label("telefoonnummer", new PropertyModel<String>(getGeselecteerdModel(), "telefoonnummer")));
		add(new Label("huisartsAgb", new PropertyModel<String>(getGeselecteerdModel(), "huisartsAgb")));
		add(new Label("praktijkAgb", new PropertyModel<String>(getGeselecteerdModel(), "praktijkAgb")));
		add(new Label("klantnummer", new PropertyModel<String>(getGeselecteerdModel(), "klantnummer")));
		add(new Label("ediadres", new PropertyModel<String>(getGeselecteerdModel(), "oorspronkelijkEdiadres")));
		add(new Label("communicatieadres", new PropertyModel<>(getGeselecteerdModel(), "ediadres")));
		add(new AjaxLink<ColonScreeningRonde>("terug", getModel())
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
				getDialog().openWith(target, new HuisartsZoekenDialogPanel(IDialog.CONTENT_ID)
				{

					@Override
					protected void close(AjaxRequestTarget target)
					{
						getDialog().close(target);
					}

					@Override
					protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts)
					{
						HuisartsVorigeRondeDialogPanel.this.getModelObject().setColonHuisarts(huisarts);
						getHuisartsWijzigenPanel().verversHuisarts(target);
						getDialog().close(target);
					}
				});
			}
		});
		add(new AjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
		add(new AjaxLink<ColonScreeningRonde>("opslaan", getModel())
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				EnovationHuisarts ha = getGeselecteerdModel().getObject();
				ColonScreeningRonde laatsteRonde = getModelObject();
				laatsteRonde.setColonHuisarts(ha);
				getHuisartsWijzigenPanel().verversHuisarts(target);
				close(target);
			}
		});
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(getGeselecteerdModel());
		ModelUtil.nullSafeDetach(getZoekModel());
	}

	protected abstract void close(AjaxRequestTarget target);

}
