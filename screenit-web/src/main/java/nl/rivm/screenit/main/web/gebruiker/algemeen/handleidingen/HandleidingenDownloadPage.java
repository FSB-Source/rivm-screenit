package nl.rivm.screenit.main.web.gebruiker.algemeen.handleidingen;

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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.DownloadLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.HANDLEIDINGEN_DOWNLOADEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class HandleidingenDownloadPage extends AlgemeenPage
{
	@SpringBean(name = "handleidingenPath")
	private String handleidingenPath;

	public HandleidingenDownloadPage()
	{
		Form<?> form = new Form<>("form");
		List<File> list = getFiles();

		ListView<File> bestandsLijst = new ListView<File>("bestandsLijst", list)
		{
			@Override
			protected void populateItem(ListItem<File> item)
			{
				File file = item.getModelObject();
				item.add(new Label("bestandsnaam", file.getName()));
				DownloadLink link = new DownloadLink("download", file);
				item.add(link);
			}

		};
		form.add(bestandsLijst);
		add(form);
	}

	public List<File> getFiles()
	{
		File path = new File(handleidingenPath);
		File[] fileArray = path.listFiles();

		List<File> fileList = new ArrayList<>();
		if (fileArray != null)
		{
			for (File file : fileArray)
			{
				if (!file.isDirectory() && !file.isHidden())
				{
					fileList.add(file);
				}
			}

		}
		return fileList;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.handleidigendownload", HandleidingenDownloadPage.class));
		return contextMenuItems;
	}

}
