package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts;

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

import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonHuisartsWijzigenPanel;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.annot.objects.Transient;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public abstract class HuisartsVorigeRondeDialogPanel extends GenericPanel<ColonScreeningRonde>
{

	private static final long serialVersionUID = 1L;

	private IModel<EnovationHuisarts> geselecteerdModel;

	@Transient
	private IModel<EnovationHuisarts> zoekModel;

	private BootstrapDialog dialog;

	private ColonHuisartsWijzigenPanel huisartsWijzigenPanel;

	public HuisartsVorigeRondeDialogPanel(String id, IModel<ColonScreeningRonde> colonScreeningRonde, IModel<EnovationHuisarts> geselecteerdModel, IModel<EnovationHuisarts> zoekModel,
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

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
				getDialog().openWith(target, new HuisartsZoekenDialogPanel(IDialog.CONTENT_ID, getModel(), getZoekModel(), getDialog(), true, getHuisartsWijzigenPanel())
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void close(AjaxRequestTarget target)
					{
						getDialog().close(target);
					}
				});
			}
		});
		add(new AjaxLink<Void>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
		add(new AjaxLink<ColonScreeningRonde>("opslaan", getModel())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				EnovationHuisarts ha = getGeselecteerdModel().getObject();
				ColonScreeningRonde laatsteRonde = getModelObject();
				laatsteRonde.setColonHuisarts(ha);
				if (laatsteRonde.getOnbekendeHuisarts() != null)
				{
					laatsteRonde.setOnbekendeHuisarts(null);
				}
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

	public IModel<EnovationHuisarts> getGeselecteerdModel()
	{
		return geselecteerdModel;
	}

	public void setGeselecteerdModel(IModel<EnovationHuisarts> geselecteerdModel)
	{
		this.geselecteerdModel = geselecteerdModel;
	}

	public IModel<EnovationHuisarts> getZoekModel()
	{
		return zoekModel;
	}

	public void setZoekModel(IModel<EnovationHuisarts> zoekModel)
	{
		this.zoekModel = zoekModel;
	}

	protected abstract void close(AjaxRequestTarget target);

	public BootstrapDialog getDialog()
	{
		return dialog;
	}

	private void setDialog(BootstrapDialog dialog)
	{
		this.dialog = dialog;
	}

	private ColonHuisartsWijzigenPanel getHuisartsWijzigenPanel()
	{
		return huisartsWijzigenPanel;
	}

	private void setHuisartsWijzigenPanel(ColonHuisartsWijzigenPanel huisartsWijzigenPanel)
	{
		this.huisartsWijzigenPanel = huisartsWijzigenPanel;
	}
}
