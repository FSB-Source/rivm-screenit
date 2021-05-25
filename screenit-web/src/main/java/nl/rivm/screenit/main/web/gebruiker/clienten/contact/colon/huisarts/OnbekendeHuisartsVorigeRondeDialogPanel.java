package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts;

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

import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonHuisartsWijzigenPanel;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.util.AdresUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public abstract class OnbekendeHuisartsVorigeRondeDialogPanel extends GenericPanel<ColonScreeningRonde>
{

	private static final long serialVersionUID = 1L;

	private IModel<OnbekendeHuisarts> ohaModel;

	private ColonHuisartsWijzigenPanel huisartsWijzigenPanel;

	private BootstrapDialog dialog;

	public OnbekendeHuisartsVorigeRondeDialogPanel(String id, IModel<ColonScreeningRonde> colonScreeningRonde, IModel<OnbekendeHuisarts> ohaModel,
		ColonHuisartsWijzigenPanel huisartsWijzigenPanel,
		BootstrapDialog dialog)
	{
		super(id, colonScreeningRonde);

		setOhaModel(ohaModel);
		setDialog(dialog);
		setHuisartsWijzigenPanel(huisartsWijzigenPanel);

		add(new Label("huisartsNaam", new PropertyModel<String>(getOhaModel(), "huisartsNaam")));
		add(new Label("praktijkNaam", new PropertyModel<String>(getOhaModel(), "praktijkNaam")));
		add(new Label("praktijkAdres", AdresUtil.getAdres(getOhaModel().getObject())));
		add(new Label("telefoonnummer", new PropertyModel<String>(getOhaModel(), "telefoonnummer")));
		add(new Label("faxnummer", new PropertyModel<String>(getOhaModel(), "faxnummer")));

		add(new IndicatingAjaxLink<ColonScreeningRonde>("opslaan", getModel())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				OnbekendeHuisarts ha = getOhaModel().getObject();
				ColonScreeningRonde laatsteRonde = getModelObject();
				laatsteRonde.setColonHuisarts(null);
				laatsteRonde.setOnbekendeHuisarts(ha);

				getHuisartsWijzigenPanel().verversHuisarts(target);
				close(target);
			}
		});
		add(new AjaxLink<ColonScreeningRonde>("annuleren", getModel())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
		add(new AjaxLink<ColonScreeningRonde>("terug", getModel())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);

				getDialog().openWith(target,
					new HuisartsZoekenDialogPanel(IDialog.CONTENT_ID, getModel(), ModelUtil.cModel(new EnovationHuisarts()), getDialog(), false, getHuisartsWijzigenPanel())
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
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(getOhaModel());
	}

	public IModel<OnbekendeHuisarts> getOhaModel()
	{
		return ohaModel;
	}

	public void setOhaModel(IModel<OnbekendeHuisarts> ohaModel)
	{
		this.ohaModel = ohaModel;
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
