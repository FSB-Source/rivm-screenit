package nl.rivm.screenit.main.web.gebruiker.algemeen.retourzending;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.RetourredenAfhandeling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.RetourzendingAfhandelingType;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.beans.support.PropertyComparator;
import org.wicketstuff.shiro.ShiroConstraint;

import com.google.common.primitives.Ints;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_RETOURREDENKOPPELEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class RetourzendingRedenBeheerPage extends RetourzendingBasePage
{

	@SpringBean
	private HibernateService hibernateService;

	private Form<RetourredenAfhandeling> retourRedenAfhandelingForm;

	private IModel<RetourredenAfhandeling> retourredenAfhandelingModel;

	private ScreenitDataTable<RetourredenAfhandeling, String> retourRedenAfhandelingTabel;

	public RetourzendingRedenBeheerPage()
	{
		RetourredenAfhandeling retourredenAfhandeling = new RetourredenAfhandeling();
		retourredenAfhandeling.setAfhandeling(RetourzendingAfhandelingType.NIEUWE_GBA_AANVRAAG);
		retourredenAfhandelingModel = ModelUtil.cModel(retourredenAfhandeling);
		retourRedenAfhandelingForm = new ScreenitForm<>("retourRedenAfhandelingForm", retourredenAfhandelingModel);
		retourRedenAfhandelingForm.setOutputMarkupId(true);
		add(retourRedenAfhandelingForm);

		TextField<String> retourRedenTextField = ComponentHelper.newTextField("retourReden", 255, true);
		retourRedenTextField.add(new UniqueFieldValidator<>(RetourredenAfhandeling.class, retourredenAfhandeling.getRetourReden(), "retourReden", hibernateService, true));
		retourRedenAfhandelingForm.add(retourRedenTextField);

		final ScreenitDropdown<RetourzendingAfhandelingType> afhandelingenDropdown = ComponentHelper.newDropDownChoice("afhandeling",
			new ListModel<>(Arrays.asList(RetourzendingAfhandelingType.values())), new EnumChoiceRenderer<RetourzendingAfhandelingType>(), true);
		afhandelingenDropdown.setOutputMarkupId(true);
		retourRedenAfhandelingForm.add(afhandelingenDropdown);

		retourRedenAfhandelingForm.add(new IndicatingAjaxButton("toevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{

				hibernateService.saveOrUpdate(retourredenAfhandelingModel.getObject());
				target.add(retourRedenAfhandelingForm, retourRedenAfhandelingTabel);
				RetourredenAfhandeling retourredenAfhandeling = new RetourredenAfhandeling();
				retourredenAfhandeling.setAfhandeling(retourredenAfhandelingModel.getObject().getAfhandeling());
				retourredenAfhandelingModel.setObject(retourredenAfhandeling);
			}

		});

		List<IColumn<RetourredenAfhandeling, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Retourreden"), "retourReden", "retourReden"));
		columns.add(new EnumPropertyColumn<>(Model.of("Afhandeling"), "afhandeling", "afhandeling"));
		columns.add(new AbstractColumn<RetourredenAfhandeling, String>(Model.of("Verwijderen"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<RetourredenAfhandeling>> cellItem, String componentId, final IModel<RetourredenAfhandeling> rowModel)
			{
				cellItem.add(new AjaxImageCellPanel<RetourredenAfhandeling>(componentId, rowModel, "icon-trash")
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void onClick(AjaxRequestTarget target)
					{
						RetourredenAfhandeling retourredenAfhandeling = rowModel.getObject();
						if (retourredenAfhandeling.getId() != null)
						{
							hibernateService.delete(retourredenAfhandeling);
						}
						target.add(retourRedenAfhandelingForm, retourRedenAfhandelingTabel);
					}
				});
			}
		});

		retourRedenAfhandelingTabel = new ScreenitDataTable<>("retourRedenAfhandelingTabel", columns, new SortableDataProvider<RetourredenAfhandeling, String>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public Iterator<? extends RetourredenAfhandeling> iterator(long first, long count)
			{
				return getRetourredenenSorted().subList(Ints.checkedCast(first), Ints.checkedCast(first + count)).iterator();
			}

			private List<RetourredenAfhandeling> getRetourredenenSorted()
			{
				List<RetourredenAfhandeling> retourredenAfhandelingen = getRetourredenen();
				SortParam<String> sortParam = getSort();
				if (sortParam != null && sortParam.getProperty() != null)
				{
					retourredenAfhandelingen.sort(new PropertyComparator<>(sortParam.getProperty(), true, sortParam.isAscending()));
				}
				return retourredenAfhandelingen;
			}

			private List<RetourredenAfhandeling> getRetourredenen()
			{
				return new ArrayList<>(hibernateService.loadAll(RetourredenAfhandeling.class));
			}

			@Override
			public long size()
			{
				return getRetourredenen().size();
			}

			@Override
			public IModel<RetourredenAfhandeling> model(RetourredenAfhandeling object)
			{
				return ModelUtil.cModel(object);
			}

		}, 10, new Model<>("retourredenen"));
		retourRedenAfhandelingTabel.setOutputMarkupId(true);
		add(retourRedenAfhandelingTabel);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(retourredenAfhandelingModel);
	}

}
