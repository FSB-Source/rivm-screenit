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

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonHuisartsWijzigenPanel;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class OnbekendeHuisartsMakenDialogPanel extends GenericPanel<ColonScreeningRonde>
{

	private static final long serialVersionUID = 1L;

	private IModel<OnbekendeHuisarts> onbekendeHuisartsModel;

	private ColonHuisartsWijzigenPanel huisartsWijzigenPanel;

	private BootstrapDialog dialog;

	public OnbekendeHuisartsMakenDialogPanel(String id, IModel<ColonScreeningRonde> colonScreeningRonde,
		ColonHuisartsWijzigenPanel huisartsWijzigenPanel, BootstrapDialog dialog)
	{
		super(id, colonScreeningRonde);
		setOnbekendeHuisartsModel(ModelUtil.cModel(new OnbekendeHuisarts()));
		setHuisartsWijzigenPanel(huisartsWijzigenPanel);
		setDialog(dialog);

		ScreenitForm<OnbekendeHuisarts> form = new ScreenitForm<OnbekendeHuisarts>("form", getOnbekendeHuisartsModel())
		{
			@Override
			public boolean isRootForm()
			{
				return true;
			}
		};
		form.add(new TextField<String>("huisartsNaam"));
		form.add(new TextField<String>("praktijkNaam"));
		form.add(new TextField<String>("praktijkAdres"));
		form.add(new TextField<String>("praktijkPostcode"));
		form.add(new TextField<String>("praktijkPlaats"));
		form.add(new TextField<String>("telefoonnummer"));
		form.add(new TextField<String>("faxnummer"));
		add(form);

		add(new AjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ColonScreeningRonde laatsteRonde = getModelObject();
				laatsteRonde.setColonHuisarts(null);
				laatsteRonde.setOnbekendeHuisarts(mapOnbekendeHuisarts(onbekendeHuisartsModel.getObject()));

				getHuisartsWijzigenPanel().verversHuisarts(target);
				close(target);
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
	}

	private OnbekendeHuisarts mapOnbekendeHuisarts(OnbekendeHuisarts onbekendeHuisartsFormObject)
	{
		OnbekendeHuisarts onbekendeHuisarts = new OnbekendeHuisarts();
		onbekendeHuisarts.setHuisartsNaam(onbekendeHuisartsFormObject.getHuisartsNaam());
		onbekendeHuisarts.setPraktijkNaam(onbekendeHuisartsFormObject.getPraktijkNaam());
		onbekendeHuisarts.setPraktijkPlaats(onbekendeHuisartsFormObject.getPraktijkPlaats());
		onbekendeHuisarts.setPraktijkAdres(onbekendeHuisartsFormObject.getPraktijkAdres());
		onbekendeHuisarts.setPraktijkPostcode(onbekendeHuisartsFormObject.getPraktijkPostcode());
		onbekendeHuisarts.setFaxnummer(onbekendeHuisartsFormObject.getFaxnummer());
		onbekendeHuisarts.setTelefoonnummer(onbekendeHuisartsFormObject.getTelefoonnummer());
		return onbekendeHuisarts;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(onbekendeHuisartsModel);
	}

	public IModel<OnbekendeHuisarts> getOnbekendeHuisartsModel()
	{
		return onbekendeHuisartsModel;
	}

	public void setOnbekendeHuisartsModel(IModel<OnbekendeHuisarts> onbekendeHuisartsModel)
	{
		this.onbekendeHuisartsModel = onbekendeHuisartsModel;
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
